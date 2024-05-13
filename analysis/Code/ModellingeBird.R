rm(list=ls())

##Libraries
library(sf)
library(purrr)
library(dplyr)
library(sjmisc)
library(tidyverse)
library(iNEXT)
library(vegan)

setwd("H:\\BirdsMentalHealth")

########FUNCTIONS
#Centroid
# calcCentroid <- function(pointList) {
#   cent<-colMeans(do.call("cbind",pointList))
#   data.frame(latitude=cent[1], longitude=cent[2])
# }
#############

##List all eBird files
EbirdFiles<-list.files("./Data/eBird", full.names = TRUE)

##Get CMAs
poly <- read_sf("./Data/lcma000b16a_e/lcma000b16a_e.shp") #2016 CMA and CA boundaries from Canadian Census (Statistics Canada, Downloaded April 14)
poly<-subset(poly, CMATYPE=="B") # only CMAs


#Loop through CMAs and eBird files
eBirdModel<-lapply(1:nrow(poly), function(i){
 print(i) 
  ###READ IN EBIRD DATA
  data<-suppressWarnings(readRDS(paste0('./eBird/ebd_in_poly_',i,'.RDS')))
  
  ###############################################################################      
  ### SUBSET CHECKLISTS BY CALLAGHAN METHODS
  data$year<-gsub("-.*", "",data$observation_date)
  data<-subset(data, year>=2007)
  #Complete checklists only
  data<-subset(data, all_species_reported=="TRUE")
  
  #From Callaghan et al 2017 via Buxton et al. (in review)
  # We  also  limited  checklists  used according to the following criteria: 
  # (1) checklists were excluded if they had associated "group-identifiers" because these represent duplicated checklists, 
  data<-data[is.na(data$group_identifier),]
  
  #(2) checklists were excluded if they recorded a travel distance > 10 km, 
  data<-subset(data, effort_distance_km<=10|is.na(effort_distance_km))
  
  #(3) checklists were included only if the recording  duration  was  between  5  and  240  minutes
  data<-subset(data, duration_minutes>=5&duration_minutes<=240)
  
  #(4)checklists were included only if they followed the "stationary,""travelling," or "exhaustive" protocols  
  data<-subset(data, protocol_type=="Stationary"|protocol_type=="Traveling"|protocol_type=="Exhaustive")
  
  ###############################################################################        
  ###REMOVE LOCATION WITH <9 CHECKLISTS AS PER CALLAGHAN
  
  #First count the number of checklists per locality
  Locations<-data%>%
    dplyr::select(common_name, observation_count, sampling_event_identifier, locality) %>%
    group_by(locality) %>% 
    summarise(Checklists = length(unique(sampling_event_identifier)))
  
  Locationsmorethan9<-subset(Locations, Checklists>=9) 
  data_complete<-subset(data, subset = locality %in% Locationsmorethan9$locality)
  
  ####################################################################   
  ## EXCLUDE SPECIES THAT OCCUR <5% OF THE TIME
  
  ## create a dataframe which removes the species that are on <=5% of
  ## checklists in a locality
  analysis_data.95 <- data_complete %>% 
    group_by(locality, common_name) %>% 
    summarise(species_count = length(common_name)) %>% 
    inner_join(Locationsmorethan9, ., by = "locality") %>% 
    mutate(percentage_of_checklists = (species_count/Checklists) * 100) %>% 
    inner_join(data_complete, ., by = c("locality", "common_name")) %>% 
    filter(percentage_of_checklists >= 5)  ## removing species that are on < 5% of checklists in a hotspot
  
  ###############################################################################        
  #####CALCULATE SPECIES DIVERSITY METRICS
  
  #Convert X to 1 (X is presence)
  analysis_data.95$observation_count<-as.numeric(gsub("X", "1", analysis_data.95$observation_count))
  
  #Restructure dataframes for species diversity analysis
  RestructureData<-analysis_data.95 %>%
    dplyr::select(common_name, observation_count, observation_date, duration_minutes, effort_distance_km, number_observers, sampling_event_identifier, locality, year) %>%
    group_by(sampling_event_identifier, common_name) %>%
    summarise(TotalCount = sum(observation_count)) %>%
    spread(common_name, TotalCount) %>%# spread the columns to a matrix form
    replace(is.na(.), 0) #replace NA with 0
    
  #Species diversity metrics
SpeciesDiv<-data.frame(cbind(unname(diversity(RestructureData[,-1], index = "shannon")),
                                                  #Total number of species  
                                 unname(specnumber(RestructureData[,-1]))))
colnames(SpeciesDiv)<-c("Shannon","Richness")
  
#Merge in Species diversity metrics and checklist data
FinalData<-analysis_data.95 %>%
  dplyr::select(common_name, observation_count, observation_date, duration_minutes, effort_distance_km, number_observers, sampling_event_identifier, locality, year, longitude, latitude) %>%
  group_by(sampling_event_identifier) %>%
  summarise(Observation_date = unique(observation_date),
            Duration_minutes=unique(duration_minutes),
            Effort_distance_km=unique(effort_distance_km),
            Number_observers=unique(number_observers),
            Locality=unique(locality),
            Longitude=unique(longitude),
            Latitude=unique(latitude),
            Year=unique(year) )%>%
  cbind(., SpeciesDiv)

FinalData$Municipality<-poly[i,]$CMANAME

#Make julian date
FinalData$JulianDay<-as.POSIXlt(FinalData$Observation_date)$yday 
FinalData$Duration_minutes_s<-scale(FinalData$Duration_minutes)
FinalData$JulianDay_s<-scale(FinalData$JulianDay)

m1<-lmer(Shannon~Duration_minutes_s+JulianDay_s+(1|Year)+(1|Locality),data=FinalData)
m2<-glmer(Richness~Duration_minutes_s+JulianDay_s+(1|Year)+(1|Locality),family=poisson,data=FinalData)

##Generate predictions
newdata<-FinalData
#June 5
newdata$JulianDay_s<-(155-attributes(FinalData$Duration_minutes_s)$`scaled:center`)/attributes(FinalData$Duration_minutes_s)$`scaled:scale`
newdata$Duration_minutes_s<-(60-attributes(FinalData$Duration_minutes_s)$`scaled:center`)/attributes(FinalData$Duration_minutes_s)$`scaled:scale`

newdata<-newdata[c("Duration_minutes_s", "JulianDay_s", "Year", "Locality")]  

newdata$Shannon_predicted<-predict(m1,newdata=newdata, type="response")
newdata$Richness_predicted<-predict(m2,newdata=newdata, type="response")

newdata$Shannon_modelR2c<-r.squaredGLMM(m1)[2]
newdata$Richness_modelR2c<-r.squaredGLMM(m2)[2,2]

newdata$Municipality<-FinalData$Municipality

Output<-plyr::ddply(newdata, c("Year", "Locality", "Municipality"), summarize, ModeledSDiv=mean(Shannon_predicted), 
              ModeledSRich=mean(Richness_predicted), Shannon_modR2c=mean(Shannon_modelR2c),
              Richness_modR2c=mean(Richness_modelR2c))

Output
})

eBirdModel2<-do.call("rbind", eBirdModel)%>%
  rename(year = Year, locality = Locality, Municipality_gen=Municipality)%>%
  mutate(year = as.numeric(year))

BirdSpeciesResults_sub<-read.csv("./Output/eBirdDiversityDistLocations.csv")

##Fix problems with slashes
eBirdModel2[grep("Sudbury", eBirdModel2$Municipality_gen),]$Municipality_gen<-
unique(BirdSpeciesResults_sub[grep("Sudbury", BirdSpeciesResults_sub$Municipality_gen),]$Municipality_gen)

eBirdModel2[grep("Ontario part", eBirdModel2$Municipality_gen),]$Municipality_gen<-
unique(BirdSpeciesResults_sub[grep("Ontario part", BirdSpeciesResults_sub$Municipality_gen),]$Municipality_gen)

eBirdModel2[grep("partie du Qu?bec", eBirdModel2$Municipality_gen),]$Municipality_gen<-
  unique(BirdSpeciesResults_sub[grep("partie du Qu?bec", BirdSpeciesResults_sub$Municipality_gen),]$Municipality_gen)

eBirdModel2[grep("1764 Slateview Cres", eBirdModel2$locality),]$locality<-
  unique(BirdSpeciesResults_sub[grep("1764 Slateview Cres", BirdSpeciesResults_sub$locality),]$locality)

eBirdModel2[grep("Rue des Confiseurs", eBirdModel2$locality),]$locality<-
  unique(BirdSpeciesResults_sub[grep("Rue des Confiseurs", BirdSpeciesResults_sub$locality),]$locality)

eBirdModel2[grep("Pointe-aux-Trembles, 52", eBirdModel2$locality),]$locality<-
  unique(BirdSpeciesResults_sub[grep("Pointe-aux-Trembles, 52", BirdSpeciesResults_sub$locality),]$locality)

eBirdModel2[grep("45,679, -73,491", eBirdModel2$locality),]$locality<-
  unique(BirdSpeciesResults_sub[grep("45,679, -73,491", BirdSpeciesResults_sub$locality),]$locality)

BirdSpeciesResults_subM<-inner_join(BirdSpeciesResults_sub, eBirdModel2, by=c("locality", "year", "Municipality_gen"))

write.csv(BirdSpeciesResults_subM[,-1], "./Output/eBirdDiversityDistLocations_wModeled.csv", row.names = FALSE)

####################models wont converge
# eBirdModel2<-do.call("rbind", eBirdModel)
# write.csv(eBirdModel2, "./BirdDiversityOutput/BirdDiversityByChecklist.csv", row.names = FALSE)
# 
# ##CREATE MODELS
# library(lme4)
# library(MuMIn)
# 
# hist(eBirdModel2$JulianDay)
# summary(eBirdModel2$Effort_distance_km)
# 
# nrow(eBirdModel2[complete.cases(eBirdModel2$Duration_minutes),])
# 
# m1<-lmer(Shannon~Duration_minutes+JulianDay+Municipality+(1|Year)+(1|Locality),data=eBirdModel2)
# m2<-glmer(Richness~Duration_minutes+JulianDay+Effort_distance_km+Number_observers+Municipality+(1|Year)+(1|Locality),family=poisson,data=eBirdModel2)






