
#Calculates species diversity and richness of birds in nearest location to each postal code

rm(list=ls())

##Libraries
library(sf)
library(purrr)
library(dplyr)
library(sjmisc)
library(tidyverse)
library(iNEXT)

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

##Read in postal codes
# postal_old<- read_sf('./Postal codes/OneDrive_1_2-13-2023/complete_postal.shp') #postal code polygons with Regina included, fixed by Emma Feb 2023
postal<- read_sf('./Data/Dissolved/postal_dissolved.shp') #postal code polygons dissolved, fixed by Emma March 2023
postal<- st_transform(postal, st_crs(poly))
postal<- st_make_valid(postal)

#Loop through CMAs and eBird files
for (i in c(1:nrow(poly)))
{
    ###Get postal codes in the CMA
    postal_CMA<-st_filter(postal, poly[i,])
    
    #Catch CMAs with no postal codes - Regina fixed Feb 2023
    
    # if(nrow(postal_CMA)!=0){
      
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
     dplyr::select(common_name, observation_count, sampling_event_identifier, locality, year) %>%
     group_by(paste(locality, year, sep="_"), common_name) %>%
     summarise(TotalCount = sum(observation_count)) %>%
     spread(common_name, TotalCount) %>%# spread the columns to a matrix form
     replace(is.na(.), 0) %>% #replace NA with 0
     rotate_df() %>% #transpose dataframe
     set_names(unlist(.[1,])) %>% #set locality as column name
     slice(-1)
   
   RestructureData <- lapply(RestructureData, function(x) as.numeric(gsub(" ", "", x)))
   
   #Calculate species diversity for each postal code
   SpDiv<- iNEXT(RestructureData, datatype = "abundance", conf = 0.95, q=c(0,1), size=17)
   
   #subset to richness and diversity
   results<- subset(SpDiv$AsyEst, Diversity=="Species richness"|Diversity=="Shannon diversity")
   
   #Rarefied species richness and diversity
   results2<-subset(SpDiv$iNextEst$size_based, m==17)[,c("Assemblage", "Order.q", "qD", "qD.LCL", "qD.UCL")]
   results2$Diversity<-ifelse(results2$Order.q==0, "Species richness", "Shannon diversity")
   results2<-results2[,-2]
   
   Locations<-data_complete%>%
     dplyr::select(common_name, observation_count, sampling_event_identifier, locality, year) %>%
     group_by(locality, year) %>% 
     summarise(Checklists = length(unique(sampling_event_identifier)))
   
   #Join everything together
   resultsFinal<-inner_join(results, results2, by=c("Assemblage", "Diversity"))%>%
     mutate(locality = str_split(Assemblage, "_", simplify = TRUE)[ , 1])%>%
     mutate(year = str_split(Assemblage, "_", simplify = TRUE)[ , 2])%>%
     inner_join(., Locations , by = c("locality", "year"))
   
   
  ##Find the centroid of each location in the CMA - TAKES WAY TOO LONG
  # Locations<-lapply(1:length(unique(data$locality)), function(s){
  #   Locs<-subset(data, locality==unique(data$locality)[s])
  #   if(nrow(Locs>=9)){
  #     Centroid<-calcCentroid(list(Locs$latitude, Locs$longitude))
  #     data.frame(latitude=Centroid[1], longitude=Centroid[2], locality=unique(data$locality)[s])
  #   }
  # })
  # Locations2<-do.call("rbind", Locations)
  
###############################################################################        
#####FIND THE NEAREST CHECKLIST LOCATION TO EACH POSTAL CODE  
##### AND THE DISTANCE TO THE LOCATION
   
   data_sp<- analysis_data.95 %>%
   st_as_sf( coords = c("longitude", "latitude"), crs = 4326)
   data_sp<- st_transform(data_sp, st_crs(poly)) 
   
   NearestLocation<-st_nearest_feature(postal_CMA, data_sp)
   postal_CMA$DistancetoLocation <- st_distance(postal_CMA, data_sp[NearestLocation,], by_element=TRUE)
  
   #clean up
   rm(data_sp, data, data_complete)
   postal_CMA<-st_drop_geometry(postal_CMA)
   
  #Join up locality and species diversity metrics
   postal_CMA$locality<-analysis_data.95[NearestLocation,]$locality
   postal_CMA2<-inner_join(postal_CMA, resultsFinal, by="locality")

###############################################################################        
#####CLEAN AND WRITE
   
   #Clean up dataframe
   postal_CMA2<-postal_CMA2[c("POSTALCODE","MUNICIPAL","PROV","DistancetoLocation","locality","year","Diversity",         
                         "Observed","Estimator", "LCL","UCL","qD" , "qD.LCL", 
                         "qD.UCL" , "Checklists" )]
  
   colnames(postal_CMA2)<-c("POSTALCODE","MUNICIPAL","PROV","DistancetoLocation","locality","year","DiversityMetric",         
                            "Observed","ChaoEstimated","Chao_LCL","Chao_UCL","RarefiedEstimated" , "Rarefied_LCL", 
                            "Rarefied_UCL" , "NumChecklists")
   postal_CMA2$Municipality_gen<-gsub("\\/.*", "",poly$CMANAME[i])
     
  saveRDS(postal_CMA2, paste0("./eBird_Distance/BirdSpDiv_CMA_", gsub("\\/.*", "",poly$CMANAME[i]),".RDS"))
    # }
}



##List all  files and read back in
EbirdDistFiles<-list.files("./eBird_Distance", full.names = TRUE)

EbirdDistFiles_data<-lapply(1:length(EbirdDistFiles), function(i){
readRDS(EbirdDistFiles[i])
})
EbirdDistFiles_data<-do.call("rbind", EbirdDistFiles_data)

##Clean dataframe
BirdSpeciesResults<-dplyr::left_join(subset(EbirdDistFiles_data, DiversityMetric=="Species richness")[,-3],
                                     subset(EbirdDistFiles_data, DiversityMetric=="Shannon diversity")[,-3],
                                     by = c("POSTALCODE", "MUNICIPAL","Municipality_gen","year","locality","DistancetoLocation", "NumChecklists"))%>%
  rename(Observed_SpRich=Observed.x)%>%
  rename(ChaoEstimated_SpRich=ChaoEstimated.x)%>%
  rename(Chao_LCL_SpRich=Chao_LCL.x)%>%
  rename(Chao_UCL_SpRich=Chao_UCL.x)%>%
  rename(RarefiedEstimated_SpRich=RarefiedEstimated.x)%>%
  rename(Rarefied_LCL_SpRich=Rarefied_LCL.x)%>%
  rename(Rarefied_UCL_SpRich=Rarefied_UCL.x)%>%
  rename(Observed_SpDiv=Observed.y)%>%
  rename(ChaoEstimated_SpDiv=ChaoEstimated.y)%>%
  rename(Chao_LCL_SpDiv=Chao_LCL.y)%>%
  rename(Chao_UCL_SpDiv=Chao_UCL.y)%>%
  rename(RarefiedEstimated_SpDiv=RarefiedEstimated.y)%>%
  rename(Rarefied_LCL_SpDiv=Rarefied_LCL.y)%>%
  rename(Rarefied_UCL_SpDiv=Rarefied_UCL.y)%>%
  rename(Municipality=MUNICIPAL)%>%
  relocate(c("Municipality_gen", "year", "NumChecklists"), .after = "Municipality")%>%
  relocate(c("locality", "DistancetoLocation"), .after = "POSTALCODE")%>%
  mutate_at(vars(matches("SpDiv")), log)%>%#log transform Sp div measures
  select( -c("DiversityMetric.x", "DiversityMetric.y"))

length(unique(BirdSpeciesResults$POSTALCODE))#608588 postal codes
BirdSpeciesResults_sub<-subset(BirdSpeciesResults, NumChecklists>=9)


#Take out locations >1060 m 
#https://www.census.gov/content/dam/Census/programs-surveys/ahs/working-papers/how_big_is_your_neighborhood.pdf
BirdSpeciesResults_sub$DistancetoLocation<-as.numeric(BirdSpeciesResults_sub$DistancetoLocation)
BirdSpeciesResults_sub<-subset(BirdSpeciesResults_sub, DistancetoLocation<=1060)
summary(BirdSpeciesResults_sub$DistancetoLocation)

length(unique(BirdSpeciesResults_sub$POSTALCODE))#370750 postal codes

write.csv(BirdSpeciesResults_sub, "./Output/eBirdDiversityDistLocations.csv")
