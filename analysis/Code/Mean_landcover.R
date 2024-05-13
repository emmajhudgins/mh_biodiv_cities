
########################################################################################################
###############             CODE THAT EXTRACTS MEAN LANDCOVER                               ##########
###############             IN EACH POSTAL CODE POLYGON                                  ##########
########################################################################################################


rm(list=ls())

library(sf)
library(raster)
library(stars)
library(geobgu)

setwd("H:\\BirdsMentalHealth")

Landcover<-raster(".\\landcover\\CAN_NALCMS_landcover_2015v2_30m.tif")
# Landcover_s<-read_stars(".\\landcover\\CAN_NALCMS_landcover_2015v2_30m.tif")

# Value 1, Temperate or sub-polar needleleaf forest, RGB 0 61 0;
# Value 2, Sub-polar taiga needleleaf forest, RGB 148 156 112;
# Value 3, Tropical or sub-tropical broadleaf evergreen forest, RGB 0 99 0;
# Value 4, Tropical or sub-tropical broadleaf deciduous forest, RGB 30 171 5;
# Value 5, Temperate or sub-polar broadleaf deciduous forest, RGB 20 140 61;
# Value 6, Mixed forest, RGB 92 117 43;
# Value 7, Tropical or sub-tropical shrubland, RGB 179 158 43;
# Value 8, Temperate or sub-polar shrubland, RGB 179 138 51;
# Value 9, Tropical or sub-tropical grassland, RGB 232 220 94;
# Value 10, Temperate or sub-polar grassland, RGB 225 207 138;
# Value 11, Sub-polar or polar shrubland-lichen-moss, RGB 156 117 84;
# Value 12, Sub-polar or polar grassland-lichen-moss, RGB 186 212 143;
# Value 13, Sub-polar or polar barren-lichen-moss, RGB 64 138 112;
# Value 14, Wetland, RGB 107 163 138;
# Value 15, Cropland, RGB 230 174 102;
# Value 16, Barren lands, RGB 168 171 174;
# Value 17, Urban, RGB 220 33 38;
# Value 18, Water, RGB 76 112 163;
# Value 19, Snow and Ice, RGB 255 250 255.

##Get CMAs
poly <- read_sf("./lcma000b16a_e/lcma000b16a_e.shp") #2016 CMA and CA boundaries from Canadian Census (Statistics Canada, Downloaded April 14)
poly<-subset(poly, CMATYPE=="B") # only CMAs

##Read in postal codes
# postal<- read_sf('./Postal codes/OneDrive_1_2-13-2023/complete_postal.shp') #postal code polygons with Regina included, fixed by Emma Feb 2023
postal<- read_sf('./Postal codes/Dissolved/postal_dissolved.shp') #postal code polygons dissolved, fixed by Emma March 2023
postal<- st_transform(postal, st_crs(poly))
postal<- st_make_valid(postal)

###Get postal codes in the CMA
postal_CMA<-st_filter(postal, poly)
postal_CMA<- st_transform(postal_CMA, st_crs(Landcover))

rm(poly, postal)

#################################################################

LandSat<-lapply(seq(nrow(postal_CMA)), function(i){
  
  print(i)
  
  #Subset to just one postal code
  Attribute1<-postal_CMA[i,]
  
  #Get landsat values in extent of postal code
  ExtractedLS<-raster::extract(Landcover, Attribute1)
  ExtractedLS<-unlist(ExtractedLS)
  
  #Proportion blue
  #Bluespace rasters = 14,18
  Blue<-length(which(ExtractedLS==14|ExtractedLS==18))/length(ExtractedLS)
  
  #Proportion green
  #Vegetation rasters = 1-13
  Green<-length(which(ExtractedLS>=1&ExtractedLS<=13))/length(ExtractedLS)
  
  FinalData<-st_drop_geometry(Attribute1[c("POSTALCODE", "MUNICIPAL", "PROV")])%>%
    mutate(PropBlue=Blue, PropGreen=Green)
  
  FinalData
})

LandSat_results<-do.call("rbind", LandSat)
write.csv(LandSat_results, ".\\Output\\LandsatPropsinPCodes_Update.csv", row.names = FALSE)
