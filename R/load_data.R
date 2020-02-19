#' @title Get Data of  Postulaciones
#' @description Imports data of type .Rda prevously created and saved in local pc. Also import demre.csv
#' 
#' @examples 
#'
#' @export
get_data_postulaciones <- function(){
  print("HES: Loading datos_demre.rda...")
  start_time = Sys.time()
  print(start_time)
  if(file.exists("Dropouts_Chile/datos_demre.rda"))
    load("Dropouts_Chile/datos_demre.rda")  #pc Joselina
  else if(file.exists("../../Chilean Education Projects/Dropouts/Data/datos_demre_CHILE.rda")) #pc Miguel
    load("../../Chilean Education Projects/Dropouts/Data/datos_demre_CHILE.rda")
  end_time = Sys.time()
  print("... loaded. Time:")
  print(end_time)
  demre <- get_demre()
  print("HES: Merging with Demre (BigGroup).")
  data_postulaciones<-merge(data_postulaciones_,demre,by.x ="DegreeCode",by.y="DEMRE.Code" )
  print("HES: Creating ID-Year key.")
  data_postulaciones$IDYEAR<-paste0(data_postulaciones$ID,data_postulaciones$Year)
  print("HES: Creating ID-Year key.")
  return(data_postulaciones)
}


#' @title Get Data of students clasified
#' @description Load data of students. Filter only enrolled and situation(P), then classify among dropped and not dropeed. 
#' 
#' @examples 
#' 
#' @export
get_classification <- function(n=0){
  start_time = Sys.time()
  print(paste("HES: Loading datos_demre.rda...", start_time))
  if(file.exists("Dropouts_Chile/datos_demre.rda"))
    load("Dropouts_Chile/datos_demre.rda")  #pc Joselina
  else if(file.exists("../../Chilean Education Projects/Dropouts/Data/datos_demre_CHILE.rda")) #pc Miguel
    load("../../Chilean Education Projects/Dropouts/Data/datos_demre_CHILE.rda")
  end_time = Sys.time()
  print(paste("HES: ...loaded .rdata"))
  print(end_time-start_time)
  
  rm(data_postulaciones_)
  
  print("HES: Creating unique ID (ID-YEAR) for data_alumno dataset..")
  application<-data_alumno
  application$IDYEAR<-paste0(application$ID,application$Year)
  
  print("HES: Filtering enrolled...")
  #only enrolled
  filled<-subset(application,(Candidate_Situation=='P' & !is.na(Enrolled_Preference)))
  
  
  rm(data_alumno)
  rm(application)
  
  
  
  if(n){
    print(paste("HES: Subseting dataframe FILLED to ", n, "registers..."))
    filled <- sample_n(filled, n, replace = TRUE )
    print("...subseted.")
  }
  
  print(paste("HES: Creating classification, dropped not-dropped (time consuming). Number of rows of Dataframe", nrow(filled)))
  start_time <- Sys.time()
  print(start_time)
  classification<-   filled %>% 
    #filter(Year>=2012) %>% 
    group_by(ID) %>% 
    count(ID) %>% 
    ungroup() %>%
    inner_join(filled,by="ID") %>%
    group_by(ID,n) %>%
    mutate(ordered_application=seq(1,n(),1)[rank(c(Year))]) %>%
    ungroup()
  
  end_time <- Sys.time()
  print(paste("HES:Time used to process Classification "))
  print(end_time-start_time)
  return(classification)
}

#' @title Get demre
#' @description Load data from demre.csv. It is a file previously curated manually. It includes BigGroup field and Stem Field. 
#' 
#' @examples 
#' 
#' @export
get_demre <- function()
{
  if(file.exists("Dropouts_Chile/Data_Droppout/demre.csv"))
    archivo = "Dropouts_Chile/Data_Droppout/demre.csv"
  else if (file.exists("../../Chilean Education Projects/Dropouts/Data/demre.csv")) 
    archivo = "../../Chilean Education Projects/Dropouts/Data/demre.csv"
  
  demre<-read.csv(archivo, header = TRUE, sep = ",",stringsAsFactors = T,encoding = 'UTF-8')
  
  # Quitar duplicados
  demre<-demre[!duplicated(demre$DEMRE.Code), ]
  return(demre)
}


#' @title Get Stem
#' @description Using demre data, previously computed, adds Stem programs.
#' 
#' @examples 
#' 
#' @export
get_stem <- function(demre)
{
  # stem
  s<-subset(demre,select=c(DEMRE.Code,STEM,Big.Group))
  s$STEM<-ifelse(s$STEM==1,'Yes','No')
  stem<-s #previously used stem but not possible use because it is a global variable of graphics
  
  return(stem)
}


#' @title Get Alumno coe
#' @description Alumnos merged with postulaciones merged with coherence.
#' 
#' @examples 
#' 
#' @export
get_alumno_coe <- function(g=ches1217, classification, data_postulaciones){
  print("HES: getting distances.")
  distance_mat<-distances(g)
  print("HES: Getting data_alumno_coe, time:")
  start_time = Sys.time()
  print(start_time)
  data_alumno_coe<-classification %>% 
    inner_join(data_postulaciones,by=c("ID","Year","IDYEAR")) %>%
    filter(Big.Group %in% V(g)$name) %>% #filtering the degree programs in the HES
    group_by(IDYEAR) %>%
    summarise(coherence=ifelse(try(sum(distance_mat[unique(as.character(Big.Group)),unique(as.character(Big.Group))])/(n()*(n()-1)))=="try-error",NA,sum(distance_mat[unique(as.character(Big.Group)),unique(as.character(Big.Group))])/(n()*(n()-1)))) %>%
    ungroup() %>%
    inner_join(classification,by="IDYEAR")
  end_time = Sys.time()
  print(end_time)
  print(end_time-start_time)
  
  
  data_alumno_coe<- data_alumno_coe %>% mutate(f_ind=case_when(n==1 ~ "Non-dropped",
                                                               (n!=1 & ordered_application==1) ~ "First dropped"
  ),
  times_applied=ifelse(n==1,"Once",'Twice or More'),
  decision=case_when(ordered_application<n  ~"Dropped",
                     ordered_application==n ~ "Final"),
  dropped=case_when((n==1 & ordered_application==1)~"0 None",
                    (n>=2 & ordered_application==1)~"1 Once",
                    (n==2 & ordered_application==2)~"0 None",
                    (n>=3 & ordered_application==2)~"1 Once",
                    (n==3 & ordered_application==3)~"0 None",
                    (n>=4 & ordered_application==3)~"1 Once",
                    (n==4 & ordered_application==4)~"0 None",
                    (n>=5 & ordered_application==4)~"1 Once",
                    (n==5 & ordered_application==5)~"0 None",
                    (n>=6 & ordered_application==5)~"1 Once",
                    (n==6 & ordered_application==6)~"0 None",
                    (n>=7 & ordered_application==6)~"1 Once",
                    (n==7 & ordered_application==7)~"0 None",
                    (n>=8 & ordered_application==7)~"1 Once",
                    (n==8 & ordered_application==8)~"0 None",
                    (n>=9 & ordered_application==8)~"1 Once",
                    (n==9 & ordered_application==9)~"0 None",
                    (n>=10 & ordered_application==9)~"1 Once",
                    (n==10 & ordered_application==10)~"0 None"))
  
  
  return (data_alumno_coe)
}
