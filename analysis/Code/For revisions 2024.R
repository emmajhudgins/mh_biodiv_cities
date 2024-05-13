


library(sf)

setwd("D:\\BirdsMentalHealth")

##Read in postal code area shapefile (made by E Hudgins in QGIS)
PostalCodeAreas = st_read("./Data/Postal code area/postalcode_areas.shp")

##Read in postal codes limited to CMAs
PostalCodeswData<-read.csv(".\\Output\\eBirdDiversityDistLocations.csv")
UniquePostalCodes<-unique(PostalCodeswData$POSTALCODE)

#Narrow postal codes to those that have eBird data and are <16km2
PostalCodeAreas_eBird<-dplyr::filter(PostalCodeAreas, POSTALCODE %in% UniquePostalCodes)
PostalCodeAreas_eBird<-subset(PostalCodeAreas_eBird, Area_m<=1000*16)


summary(PostalCodeAreas_eBird$Area_m/1000)
hist(PostalCodeAreas_eBird$Area_m/1000)
