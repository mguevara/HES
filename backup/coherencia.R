g = ches1217
distance_mat<-distances(g)

data_alumno_coe<-classification %>% 
  inner_join(data_postulaciones_,by=c("ID","Year","IDYEAR")) %>%
  filter(Big.Group %in% V(g)$name) %>% #filtering the degree programs in the HES
  group_by(IDYEAR) %>%
  summarise(coherence=ifelse(try(sum(distance_mat[unique(as.character(Big.Group)),unique(as.character(Big.Group))])/(n()*(n()-1)))=="try-error",NA,sum(distance_mat[unique(as.character(Big.Group)),unique(as.character(Big.Group))])/(n()*(n()-1)))) %>%
  ungroup() %>%
  inner_join(classification,by="IDYEAR")

#inco$dist[inco$dist==0]<-NA
#inco$sepa<-paste0(inco$Drop," ",inco$Application)
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