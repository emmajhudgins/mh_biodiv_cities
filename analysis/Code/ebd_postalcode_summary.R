# Aggregate eBird checklist data by Canadian Census Block Divisions
# written by Emma J. Hudgins April 16, 2021 _ Altered by Rachel Buxton March 2023

# require(devtools)
# install_github("CornellLabofOrnithology/auk")
require(sf)
require(auk)
require(dplyr)
write=F

setwd("H:\\BirdsMentalHealth")

poly <- read_sf("./Data/lcma000b16a_e/lcma000b16a_e.shp") #2016 CMA and CA boundaries from Canadian Census (Statistics Canada, Downloaded April 14)
poly<-subset(poly, CMATYPE=="B") # only CMAs

postal<- read_sf('./Data/Dissolved/postal_dissolved.shp') #postal code polygons dissolved, fixed by Emma March 2023
postal<- st_transform(postal, st_crs(poly))
postal<-st_make_valid(postal)


for (i in 1:nrow(poly))
{
  # start<-Sys.time()
  #Join up postal codes
  postal_CMA<-st_filter(postal, poly[i,])
  data<-readRDS(paste0('./eBird/ebd_in_poly_',i,'.RDS'))
  data<- data %>%
  st_as_sf( coords = c("longitude", "latitude"), crs = 4326)
  data<- st_transform(data, st_crs(poly))
  data<-data%>%st_join(postal_CMA)
  
  #Subset year
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
  
  # remove data not associated with postalcodes
  data<-data[is.na(data$POSTALCODE)==F,]
  
  #Remove year * postal codes with <9 checklists
  data_sum<-data%>%
    group_by(POSTALCODE, year)%>%summarise_at(c('checklist_id', 'scientific_name'),n_distinct)
  colnames(data_sum)[3:4]<-c("n_checklists", "species_richness")
  data_sum2<-data_sum%>%group_by(POSTALCODE)%>%summarise_at('n_checklists',max)
  data_sum2<-subset(data_sum2, n_checklists>=9)
  
  data<-subset(data, POSTALCODE%in%data_sum2$POSTALCODE)
  
  saveRDS(as.data.frame(data[,c(1,6:7,8,25:35,44:62)]), paste0("./Data/OneDrive_1_1-20-2023/ebd_cleaned_CMA_", gsub("\\/.*", "",poly$CMANAME[i]),".RDS"))
  
  # end<-Sys.time()    
  # end-start
}
