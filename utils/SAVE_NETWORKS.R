#THIS SCRIPT SAVES .RDATA OF EACH NETWORK

create_network_mg <- function(nodes, edges){
  library(stringi)
  library(igraph)
  
  nodes$Description <- stri_trans_general(nodes$Description, "latin-ascii")
  colnames(edges) <- c("c1","c2", "used")
  
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes )
  g <- delete.edges(g, which(E(g)$used < 1))
  plot(g)
  str(g)
  
  #from my paper on diversity
  #V(g)$size = log(totals[match(V(g)$name, names(totals))], base = 2) - 9
  V(g)$size = V(g)$degree 
  fc <- fastgreedy.community(g); colors <- rainbow(max(membership(fc)))
  V(g)$color = colors[membership(fc)]
  set.seed(67)
  V(g)$label <- V(g)$Description
  g$layout <- layout.fruchterman.reingold(g)
  plot.igraph(g,  vertex.label.cex = 0.5, vertex.label.font = 1, vertex.label.family = "Helvetica", vertex.label.color="black", asp =FALSE)
  
  return(g)
}

create_network <- function(nodes, edges){
  library(stringi)
  library(igraph)
  library(RColorBrewer)
  
  #working with nodes
  chile_1217<-nodes
  demre<-get_demre()
  
  stem<- get_stem(demre)
  
  chile_1217<-merge(chile_1217,stem,by.x ='ID',by.y = 'DEMRE.Code',all.x=T)
  
  data.ed<-chile_1217
  ix <- 3:31 #This depends on the dataframe, so be careful
  data.ed[ix] <- lapply(data.ed[ix], as.numeric) 
  names(data.ed)<-gsub('\\.',"",names(data.ed))
  data.ed$lbetw<-log(data.ed$betweeness)
  data.ed$lbetw[is.infinite(data.ed$lbetw)]<-NA
  
  #working with edges
  chile_1217<-subset(edges,y==1,select = -y)
  rownames(chile_1217)<-1:length(chile_1217$i)
  chile_1217$i<-as.factor(chile_1217$i);chile_1217$j<-as.factor(chile_1217$j)
  g<-graph_from_edgelist(as.matrix(chile_1217),directed = F)
  
  #V(g)$name<-data.ed$Description[match(V(g)$name,data.ed$ID)]
  #heck factors!
  V(g)$name<-as.character(data.ed$Description[match(V(g)$name,as.character(data.ed$ID))])
  V(g)$gender<-data.ed$Gender2017_Std[match(V(g)$name,data.ed$Description)]
  V(g)$stem<-data.ed$STEM[match(V(g)$name,data.ed$Description)]
  V(g)$label.cex=(sqrt(degree(g)))/4;V(g)$label.cex[V(g)$label.cex<0.75]=0.00001
  V(g)$score<-data.ed$Scores2017_Std[match(V(g)$name,data.ed$Description)]
  V(g)$community<-data.ed$Community[match(V(g)$name,data.ed$Description)]
  
  
  components = V(g)$gender
  #my.col <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(diff(range(V(g)$gender)*2))
  my.col <- colorRampPalette(rev(brewer.pal(11, "RdBu")))
  #V(g)$color_gender <- my.col[(components+ abs(min ( components ,na.rm = T))+1)*2]#COLOUR
  
  components = V(g)$stem
  my.col <- colorRampPalette(brewer.pal(3, "RdBu"))(length(unique(range(V(g)$stem))))
  V(g)$color_stem <- my.col[ifelse(components=='No',1,2)]#COLOUR
  
  components = V(g)$stem
  my.col <- c('circle','square')
  V(g)$shape_stem <- my.col[ifelse(components=='Yes',1,2)]#COLOUR
  
  #components = V(g)$score
  #my.col <- colorRampPalette(rev(brewer.pal(6, "RdBu")))(diff(range(V(g)$score)))
  #V(g)$color_score <- my.col[(components+ abs(min ( components ,na.rm = T))+1)]#COLOUR
  
  components = V(g)$community
  my.col <- colorRampPalette(brewer.pal(11, "RdBu"))(length(unique(V(g)$community)))
  V(g)$color_comunity <- my.col[components]#COLOUR
  
  return(g)
}


#CHILE 2006-2011
edges <- read.csv("~/Dropbox/UPLA/INVESTIGACION/PROYECTOS/FONDEF\ CHES/Chilean\ Education\ Projects/Dropouts/Data/Chilean\ Network\ 1/Adjancency_Chile_0611_Jan2020.csv")

nodes <- read.csv("~/Dropbox/UPLA/INVESTIGACION/PROYECTOS/FONDEF\ CHES/Chilean\ Education\ Projects/Dropouts/Data/Chilean\ Network\ 1/ChileData_0611_Jan2020.csv", header=TRUE)

ches0611 <- create_network(nodes,edges)
save(ches0611, file = "data/ches0611.RData")

#CHILE 2012-2017################ 
edges <- read.csv("~/Dropbox/UPLA/INVESTIGACION/PROYECTOS/FONDEF\ CHES/Chilean\ Education\ Projects/Dropouts/Data/Chilean\ Network\ 2/Adjancency_Chile_1217_Jan2020.csv")

nodes <- read.csv("~/Dropbox/UPLA/INVESTIGACION/PROYECTOS/FONDEF\ CHES/Chilean\ Education\ Projects/Dropouts/Data/Chilean\ Network\ 2/ChileData_1217_Jan2020.csv", header=TRUE)

ches1217 <- create_network(nodes,edges)
save(ches1217, file = "data/ches1217.RData")


#testing visNetwork
#from notebook



