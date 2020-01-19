chile_1217<-read.csv("Dropouts_Chile/Data_Droppout/ChileData_1217.csv", header = TRUE, sep = ",",stringsAsFactors = F)
chile_1217<-merge(chile_1217,stem,by.x ='ID',by.y = 'DEMRE.Code',all.x=T)

data.ed<-chile_1217
ix <- 3:31 #This depends on the dataframe, so be careful
data.ed[ix] <- lapply(data.ed[ix], as.numeric) 
names(data.ed)<-gsub('\\.',"",names(data.ed))
data.ed$lbetw<-log(data.ed$betweeness);data.ed$lbetw[is.infinite(data.ed$lbetw)]<-NA