# premanipulating

## nuevas variables
application <- data_alumno # generacion nueva tabla
application$IDYEAR <- paste0(application$ID,application$Year) # nueva variable IdYear

# all enrolled: quita los candidatos en situación P y con na en enrolled preference

filled <- subset(application,(Candidate_Situation=='P' & !is.na(Enrolled_Preference)))
#str(filled)
#length(unique(filled$ID))
#app2010<-app2010[!duplicated(app2010$IDYEAR), ]

# separating dropped no-dropped

classification<-   filled %>% 
  #filter(Year>=2012) %>% 
  group_by(ID) %>% 
  count(ID) %>% 
  ungroup() %>%
  inner_join(filled,by="ID") %>%
  group_by(ID,n) %>%
  #mutate(ordered_application=seq(1,n(),1)[rank(c(Year))]) %>%
  ungroup()

classification2 <- classification %>%
  mutate(ordered_application=seq(1,n(),1)[rank(c(Year))]) %>%
  ungroup()





