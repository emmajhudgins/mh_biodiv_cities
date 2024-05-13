
#Models exploring the relationship between species diversity of birds and trees and 
#different metrics of marginalization

rm(list=ls())

library(tidyverse)
library(dplyr)
library(pscl)

setwd("H:\\BirdsMentalHealth")

#Read in CANMARG data
CANMARG<-read.csv("Data\\CanadianMarginalizationIndex\\cmg_a_16.csv", header=TRUE)%>%
  dplyr::rename(POSTALCODE = `postalcode16`)%>%
  dplyr::select(POSTALCODE,cmg16_10, cmg16_11, cmg16_12,cmg16_13)

##Read in bird species diversity in postal codes
BirdDivPostalCode<-read.csv("Output\\BirdSpeciesDiversityByPostalCode.csv", header=TRUE)%>%
  inner_join(., CANMARG, by="POSTALCODE")%>%#Merge in CANMARG
  filter(year==2016)%>%
  filter(cmg16_10 > -9999)

##Read in bird species diversity in localities near postal codes
BirdDivDistLoc<-read.csv("Output\\eBirdDiversityDistLocations.csv", header=TRUE)%>%
inner_join(., CANMARG, by="POSTALCODE")%>%#Merge in CANMARG
  filter(year==2016)%>%
  filter(cmg16_10 > -9999)

##Read in bird species diversity in postal codes
TreeDiversity<-readRDS("Data\\treediversity_metrics_v2.RDS")%>%
  # group_by(postal) %>%
  # summarize(mean_SR = mean(tree_richness), max_SR = max(tree_richness))%>%
  dplyr::rename(POSTALCODE="postal")%>%
  inner_join(., CANMARG, by="POSTALCODE")%>%
  filter(cmg16_10 > -9999)

cor(BirdDivPostalCode[c("cmg16_10", "cmg16_11", "cmg16_12", "cmg16_13")], method="spearman")#largest R = -0.54
cor(BirdDivDistLoc[c("cmg16_10", "cmg16_11", "cmg16_12", "cmg16_13")], method="spearman")#R = -0.447
cor(TreeDiversity[c("cmg16_10", "cmg16_11", "cmg16_12", "cmg16_13")], method="spearman")#R = -0.47

#MODELS

##########################################################################################
##########                      SUMMARIZED BY POSTAL CODE                       ##########
##########################################################################################

######TREE SPECIES RICHNESS
#richness
m1Tree<-glm(tree_richness~cmg16_10, data=TreeDiversity, family = poisson)
m2Tree<-glm(tree_richness~cmg16_11, data=TreeDiversity, family = poisson)
m3Tree<-glm(tree_richness~cmg16_12, data=TreeDiversity, family = poisson)
m4Tree<-glm(tree_richness~cmg16_13, data=TreeDiversity, family = poisson)

AIC(m1Tree,m2Tree,m3Tree,m4Tree) %>% #cmg16_10 has the lowest AIC
  arrange(AIC)

#max
m5Tree<-glm(tree_diversity~cmg16_10, data=TreeDiversity, family = gaussian)
m6Tree<-glm(tree_diversity~cmg16_11, data=TreeDiversity, family = gaussian)
m7Tree<-glm(tree_diversity~cmg16_12, data=TreeDiversity, family = gaussian)
m8Tree<-glm(tree_diversity~cmg16_13, data=TreeDiversity, family = gaussian)

AIC(m5Tree,m6Tree,m7Tree,m8Tree) %>% #cmg16_10 has the lowest AIC
  arrange(AIC)

######BIRD SPECIES RICHNESS

##Observed, summarized by postal code
m1<-glm(Observed_SpRich~cmg16_10, data=BirdDivPostalCode, family = poisson)
m2<-glm(Observed_SpRich~cmg16_11, data=BirdDivPostalCode, family = poisson)
m3<-glm(Observed_SpRich~cmg16_12, data=BirdDivPostalCode, family = poisson)
m4<-glm(Observed_SpRich~cmg16_13, data=BirdDivPostalCode, family = poisson)

AIC(m1,m2,m3,m4) %>% #cmg16_10 has the lowest AIC
  arrange(AIC)

##Observed, summarized by distance to location
m1Dist<-glm(Observed_SpRich~cmg16_10, data=BirdDivDistLoc, family = poisson)
m2Dist<-glm(Observed_SpRich~cmg16_11, data=BirdDivDistLoc, family = poisson)
m3Dist<-glm(Observed_SpRich~cmg16_12, data=BirdDivDistLoc, family = poisson)
m4Dist<-glm(Observed_SpRich~cmg16_13, data=BirdDivDistLoc, family = poisson)

AIC(m1Dist,m2Dist,m3Dist,m4Dist)%>% #cmg16_13 has the lowest AIC
  arrange(AIC)

##Chao, summarized by postal code
BirdDivPostalCode$ChaoEstimated_SpRich<-round(BirdDivPostalCode$ChaoEstimated_SpRich,0)
m5<-glm(ChaoEstimated_SpRich~cmg16_10, data=BirdDivPostalCode, family = poisson)
m6<-glm(ChaoEstimated_SpRich~cmg16_11, data=BirdDivPostalCode, family = poisson)
m7<-glm(ChaoEstimated_SpRich~cmg16_12, data=BirdDivPostalCode, family = poisson)
m8<-glm(ChaoEstimated_SpRich~cmg16_13, data=BirdDivPostalCode, family = poisson)

AIC(m5,m6,m7,m8) %>% #cmg16_10 has the lowest AIC
  arrange(AIC)

##Chao, summarized by distance to location
BirdDivDistLoc$ChaoEstimated_SpRich<-round(BirdDivDistLoc$ChaoEstimated_SpRich,0)
m5Dist<-glm(ChaoEstimated_SpRich~cmg16_10, data=BirdDivDistLoc, family = poisson)
m6Dist<-glm(ChaoEstimated_SpRich~cmg16_11, data=BirdDivDistLoc, family = poisson)
m7Dist<-glm(ChaoEstimated_SpRich~cmg16_12, data=BirdDivDistLoc, family = poisson)
m8Dist<-glm(ChaoEstimated_SpRich~cmg16_13, data=BirdDivDistLoc, family = poisson)

AIC(m5Dist,m6Dist,m7Dist,m8Dist)%>% #cmg16_13 has the lowest AIC
  arrange(AIC)

##Rarefied, summarized by postal code
BirdDivPostalCode$RarefiedEstimated_SpRich<-round(BirdDivPostalCode$RarefiedEstimated_SpRich,0)
m9<-glm(RarefiedEstimated_SpRich~cmg16_10, data=BirdDivPostalCode, family = poisson)
m10<-glm(RarefiedEstimated_SpRich~cmg16_11, data=BirdDivPostalCode, family = poisson)
m11<-glm(RarefiedEstimated_SpRich~cmg16_12, data=BirdDivPostalCode, family = poisson)
m12<-glm(RarefiedEstimated_SpRich~cmg16_13, data=BirdDivPostalCode, family = poisson)

AIC(m9,m10,m11,m12) %>% #cmg16_10 has the lowest AIC
  arrange(AIC)

##Rarefied, summarized by distance to location
BirdDivDistLoc$RarefiedEstimated_SpRich<-round(BirdDivDistLoc$RarefiedEstimated_SpRich,0)
m9Dist<-glm(RarefiedEstimated_SpRich~cmg16_10, data=BirdDivDistLoc, family = poisson)
m10Dist<-glm(RarefiedEstimated_SpRich~cmg16_11, data=BirdDivDistLoc, family = poisson)
m11Dist<-glm(RarefiedEstimated_SpRich~cmg16_12, data=BirdDivDistLoc, family = poisson)
m12Dist<-glm(RarefiedEstimated_SpRich~cmg16_13, data=BirdDivDistLoc, family = poisson)

AIC(m9Dist,m10Dist,m11Dist,m12Dist)%>% #cmg16_10 has the lowest AIC
  arrange(AIC)

######BIRD SPECIES DIVERSITY

##Observed, summarized by postal code
m13<-glm(Observed_SpDiv~cmg16_10, data=BirdDivPostalCode, family = gaussian)
m14<-glm(Observed_SpDiv~cmg16_11, data=BirdDivPostalCode, family = gaussian)
m15<-glm(Observed_SpDiv~cmg16_12, data=BirdDivPostalCode, family = gaussian)
m16<-glm(Observed_SpDiv~cmg16_13, data=BirdDivPostalCode, family = gaussian)

AIC(m13,m14,m15,m16) %>% #cmg16_10 has the lowest AIC
  arrange(AIC)

##Observed, summarized by distance to location
m13Dist<-glm(Observed_SpDiv~cmg16_10, data=BirdDivDistLoc, family = gaussian)
m14Dist<-glm(Observed_SpDiv~cmg16_11, data=BirdDivDistLoc, family = gaussian)
m15Dist<-glm(Observed_SpDiv~cmg16_12, data=BirdDivDistLoc, family = gaussian)
m16Dist<-glm(Observed_SpDiv~cmg16_13, data=BirdDivDistLoc, family = gaussian)

AIC(m13Dist,m14Dist,m15Dist,m16Dist)%>% #cmg16_10 has the lowest AIC
  arrange(AIC)

##Chao, summarized by postal code
m17<-glm(ChaoEstimated_SpDiv~cmg16_10, data=BirdDivPostalCode, family = gaussian)
m18<-glm(ChaoEstimated_SpDiv~cmg16_11, data=BirdDivPostalCode, family = gaussian)
m19<-glm(ChaoEstimated_SpDiv~cmg16_12, data=BirdDivPostalCode, family = gaussian)
m20<-glm(ChaoEstimated_SpDiv~cmg16_13, data=BirdDivPostalCode, family = gaussian)

AIC(m17,m18,m19,m20) %>% #cmg16_10 has the lowest AIC
  arrange(AIC)

##Chao, summarized by distance to location
m17Dist<-glm(ChaoEstimated_SpDiv~cmg16_10, data=BirdDivDistLoc, family = gaussian)
m18Dist<-glm(ChaoEstimated_SpDiv~cmg16_11, data=BirdDivDistLoc, family = gaussian)
m19Dist<-glm(ChaoEstimated_SpDiv~cmg16_12, data=BirdDivDistLoc, family = gaussian)
m20Dist<-glm(ChaoEstimated_SpDiv~cmg16_13, data=BirdDivDistLoc, family = gaussian)

AIC(m17Dist,m18Dist,m19Dist,m20Dist)%>% #cmg16_10 has the lowest AIC
  arrange(AIC)

##Rarefied, summarized by postal code
m21<-glm(RarefiedEstimated_SpDiv~cmg16_10, data=BirdDivPostalCode, family = gaussian)
m22<-glm(RarefiedEstimated_SpDiv~cmg16_11, data=BirdDivPostalCode, family = gaussian)
m23<-glm(RarefiedEstimated_SpDiv~cmg16_12, data=BirdDivPostalCode, family = gaussian)
m24<-glm(RarefiedEstimated_SpDiv~cmg16_13, data=BirdDivPostalCode, family = gaussian)

AIC(m21,m22,m23,m24) %>% #cmg16_10 has the lowest AIC
  arrange(AIC)

##Rarefied, summarized by distance to location
m21Dist<-glm(RarefiedEstimated_SpDiv~cmg16_10, data=BirdDivDistLoc, family = gaussian)
m22Dist<-glm(RarefiedEstimated_SpDiv~cmg16_11, data=BirdDivDistLoc, family = gaussian)
m23Dist<-glm(RarefiedEstimated_SpDiv~cmg16_12, data=BirdDivDistLoc, family = gaussian)
m24Dist<-glm(RarefiedEstimated_SpDiv~cmg16_13, data=BirdDivDistLoc, family = gaussian)

AIC(m21Dist,m22Dist,m23Dist,m24Dist)%>% #cmg16_10 has the lowest AIC
  arrange(AIC)

#4 - lowest AIC  cmg16_13
#10 - lowest AIC  cmg16_10

################################################################################

###JOIN INTO R2


RsquaredSD<-rbind(
  #Trees mean
  data.frame(cmg06_10=pR2(m1Tree)["r2ML"],
             cmg06_11=pR2(m2Tree)["r2ML"],
             cmg06_12=pR2(m3Tree)["r2ML"],
             cmg06_13=pR2(m4Tree)["r2ML"], 
             Taxon="Trees", 
             SD_index="ObervedSR",
             SummaryLevel="PCSummary"),
  #Trees max
  data.frame(cmg06_10=pR2(m5Tree)["r2ML"],
             cmg06_11=pR2(m6Tree)["r2ML"],
             cmg06_12=pR2(m7Tree)["r2ML"],
             cmg06_13=pR2(m8Tree)["r2ML"], 
             Taxon="Trees", 
             SD_index="ObervedSD",
             SummaryLevel="PCSummary"),
  #Birds
  #Species Richness
  #Observed
  data.frame(cmg06_10=pR2(m1)["r2ML"],
                 cmg06_11=pR2(m2)["r2ML"],
                 cmg06_12=pR2(m3)["r2ML"],
                 cmg06_13=pR2(m4)["r2ML"], 
                 Taxon="Birds", 
                 SD_index="ObervedSR",
                 SummaryLevel="PCSummary"),
  
  data.frame(cmg06_10=pR2(m1Dist)["r2ML"],
             cmg06_11=pR2(m2Dist)["r2ML"],
             cmg06_12=pR2(m3Dist)["r2ML"],
             cmg06_13=pR2(m4Dist)["r2ML"], 
             Taxon="Birds", 
             SD_index="ObervedSR",
             SummaryLevel="DistPC"),
  
  #Chao
  data.frame(cmg06_10=pR2(m5)["r2ML"],
             cmg06_11=pR2(m6)["r2ML"],
             cmg06_12=pR2(m7)["r2ML"],
             cmg06_13=pR2(m8)["r2ML"], 
             Taxon="Birds", 
             SD_index="ChaoSR",
             SummaryLevel="PCSummary"),
  
  data.frame(cmg06_10=pR2(m5Dist)["r2ML"],
             cmg06_11=pR2(m6Dist)["r2ML"],
             cmg06_12=pR2(m7Dist)["r2ML"],
             cmg06_13=pR2(m8Dist)["r2ML"], 
             Taxon="Birds", 
             SD_index="ChaoSR",
             SummaryLevel="DistPC"),
  
  #Rarefied
  data.frame(cmg06_10=pR2(m9)["r2ML"],
             cmg06_11=pR2(m10)["r2ML"],
             cmg06_12=pR2(m11)["r2ML"],
             cmg06_13=pR2(m12)["r2ML"], 
             Taxon="Birds", 
             SD_index="RarefiedSR",
             SummaryLevel="PCSummary"),
  
  data.frame(cmg06_10=pR2(m9Dist)["r2ML"],
             cmg06_11=pR2(m10Dist)["r2ML"],
             cmg06_12=pR2(m11Dist)["r2ML"],
             cmg06_13=pR2(m12Dist)["r2ML"], 
             Taxon="Birds", 
             SD_index="RarefiedSR",
             SummaryLevel="DistPC"),
  
  #Species Diversity
  #Observed
  data.frame(cmg06_10=pR2(m13)["r2ML"],
             cmg06_11=pR2(m14)["r2ML"],
             cmg06_12=pR2(m15)["r2ML"],
             cmg06_13=pR2(m16)["r2ML"], 
             Taxon="Birds", 
             SD_index="ObervedSD",
             SummaryLevel="PCSummary"),
  
  data.frame(cmg06_10=pR2(m13Dist)["r2ML"],
             cmg06_11=pR2(m14Dist)["r2ML"],
             cmg06_12=pR2(m15Dist)["r2ML"],
             cmg06_13=pR2(m16Dist)["r2ML"], 
             Taxon="Birds", 
             SD_index="ObervedSD",
             SummaryLevel="DistPC"),
  
  #Chao
  data.frame(cmg06_10=pR2(m17)["r2ML"],
             cmg06_11=pR2(m18)["r2ML"],
             cmg06_12=pR2(m19)["r2ML"],
             cmg06_13=pR2(m20)["r2ML"], 
             Taxon="Birds", 
             SD_index="ChaoSD",
             SummaryLevel="PCSummary"),
  
  data.frame(cmg06_10=pR2(m17Dist)["r2ML"],
             cmg06_11=pR2(m18Dist)["r2ML"],
             cmg06_12=pR2(m19Dist)["r2ML"],
             cmg06_13=pR2(m20Dist)["r2ML"], 
             Taxon="Birds", 
             SD_index="ChaoSD",
             SummaryLevel="DistPC"),
  
  #Rarefied
  data.frame(cmg06_10=pR2(m21)["r2ML"],
             cmg06_11=pR2(m22)["r2ML"],
             cmg06_12=pR2(m23)["r2ML"],
             cmg06_13=pR2(m24)["r2ML"], 
             Taxon="Birds", 
             SD_index="RarefiedSD",
             SummaryLevel="PCSummary"),
  
  data.frame(cmg06_10=pR2(m21Dist)["r2ML"],
             cmg06_11=pR2(m22Dist)["r2ML"],
             cmg06_12=pR2(m23Dist)["r2ML"],
             cmg06_13=pR2(m24Dist)["r2ML"], 
             Taxon="Birds", 
             SD_index="RarefiedSD",
             SummaryLevel="DistPC")
)

write.csv(RsquaredSD, ".\\Output\\RSquaredSpDivVsMarginalization.csv", row.names = FALSE)
