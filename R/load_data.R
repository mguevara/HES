#' @title Get Data dir
#' @description Retrieves the location of data, searching several possible directories. 
#' @return Return a string with the path to a found data directory. 
#' @examples 
#' @export
get_data_dir <- function(){
  data_dir = "../Dropouts/Data/" #dropbox shared folder
  if (dir.exists("Dropouts_Chile/"))
    data_dir = "Dropouts_Chile/"
  else if (dir.exists("../../Chilean Education Projects/Dropouts/Data/"))
    data_dir = "../../Chilean Education Projects/Dropouts/Data/"
  else if (dir.exists("../Chilean Education Projects/Dropouts/Data/"))
    data_dir = "../Chilean Education Projects/Dropouts/Data/"
  else { 
    stop("HES: No encontre el directorio de datos...No se puede continuar.")
  }
}

#' @title Get Data of  Postulaciones
#' @description Imports data of type .Rda prevously created and saved in local pc. Also import demre.csv by using function get_demre.
#' 
#' @return a dataframe with postulations.
#' 
#' @examples 
#'
#' @export
get_data_postulaciones <- function(){
  print("HES: Loading datos_demre.rda...")
  start_time = Sys.time()
  print(start_time)

  data_dir = get_data_dir()
  load(paste(data_dir, "datos_demre_CHILE.rda", sep=""))
  end_time = Sys.time()
  print("... loaded. Time:")
  print(end_time)
  
  demre <- get_demre()
  print("HES: Merging with Demre (BigGroup).")
  data_postulaciones<-merge(data_postulaciones_,demre,by.x ="DegreeCode",by.y="DEMRE.Code" )
  print("HES: Creating ID-Year key.")
  data_postulaciones$IDYEAR<-paste0(data_postulaciones$ID,data_postulaciones$Year)
  print("HES:Created ID-Year key.")
  return(data_postulaciones)
}


#' @title Get Data of students clasified
#' @description Load data of students. Filter only enrolled and situation(P), then classify among dropped and not dropeed. 
#' @return A data frame with information of students and a field useful to get dropped or not dropped information.
#' 
#' @param n Integer indicating a number to conduct sampling. If not indicated, all the data of students will be retrieved.
#' 
#' @examples 
#' 
#' @export
  get_classification <- function(n=0){
  print("HES: Loading datos_demre.rda...")
  start_time = Sys.time()
  print(start_time)
  data_dir = get_data_dir()
  load(paste(data_dir, "datos_demre_CHILE.rda", sep=""))
  end_time = Sys.time()
  print("... loaded. Time:")
  print(end_time)
  
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
#' @description Load data from demre.csv. It is a file previously curated manually. 
#' 
#' @return A data frame with information of programs or careers. It includes BigGroup field and Stem Field. 
#' 
#' @examples 
#' 
#' @export
get_demre <- function()
{
  data_dir = get_data_dir()
  archivo = paste(data_dir, "demre.csv", sep="")
  
  demre<-read.csv(archivo, header = TRUE, sep = ",",stringsAsFactors = T,encoding = 'UTF-8')
  
  # Quitar duplicados
  demre<-demre[!duplicated(demre$DEMRE.Code), ]
  return(demre)
}


#' @title Get Stem
#' @description Using demre data, previously computed, indicates Stem programs.
#' 
#' @param demre a dataframe previously got with function get_demre()
#' @return Information of Stem degree
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
#' @param g Graph containing a network of kind Higher Education Space HES. Default value is ches1227, CHilean HES for 2012-2017 dataset.
#' @param classification Dataframe with students' information previously obtained with function get_classification(). 
#' @param data_postulaciones Dataframe with students' information previously obtained with function get_data_postulaciones()
#' 
#' @return Data useful to perform regressions and analysis. It includes information of Coherence of network HES and information of dropped and not-dropped.
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
