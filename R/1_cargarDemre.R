# datos

demre<-read.csv("Dropouts_Chile/Data_Droppout/demre.csv", header = TRUE, sep = ",",stringsAsFactors = T,encoding = 'UTF-8')

# Quitar duplicados

demre<-demre[!duplicated(demre$DEMRE.Code), ]

# stem

stem<-subset(demre,select=c(DEMRE.Code,STEM,Big.Group))
stem$STEM<-ifelse(stem$STEM==1,'Yes','No')