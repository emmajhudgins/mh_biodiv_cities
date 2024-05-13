
#Explore the potential associations between total number of checklists per postal code 
## and marginalization

rm(list=ls())

library(tidyverse)
library(dplyr)
library(sjmisc)
library(rsq)

#Read in CANMARG data

CANMARG<-read.csv(".\\Data\\CanadianMarginalizationIndex\\cmg_a_06.csv", header=TRUE)
names(CANMARG)[1]<-"POSTALCODE"

############################################################################################
####################               Information about CANMARG                   #############
############################################################################################

# CMGYY_06 - Quintiles of Instability
# Quintiles of instability (1 = least unstable, 5 = most unstable)
# CMGYY_07 - Quintiles of Deprivation
# Quintiles of deprivation (1 = leat deprived, 5 = most deprived)
# CMGYY_08 - Quintiles of Dependency
# Quintiles of dependency (1 = least dependent, 5 = most dependent)
# CMGYY_09 - Quintiles of Ethnic Concentration
# Quintiles of ethnic concentration (1 = least ethnically concentrated, 5 = most ethnically concentrated)
# CMGYY_10 - Principal Component Factor Score - Instability
# Princial component factor score - instability (higher values = higher instability)
# CMGYY_11 - Principal Component Factor Score - Deprivation
# Princial component factor score - deprivation (higher values = higher deprivation)
# CMGYY_12 - Principal Component Factor Score - Dependency
# Principal component factor score - dependency (higher values = higher dependency)
# CMGYY_13 - Principal Component Factor Score - Ethnic Concentration
# Principal component factor score - ethnic concentration(higher values = higher ethnic concentration)

#List all ebird files for CMAs
EbirdFiles<-list.files("./OneDrive_1_1-20-2023", full.names = TRUE)

##EBird checklists

Checklists<-lapply(1:length(EbirdFiles), function(ss){
  print(ss)
  File1<-readRDS(EbirdFiles[ss])
  File1$observation_count<-as.numeric(gsub("X", "0", File1$observation_count))
  
  #First count the number of checklists per postal code
  Checklists<-File1%>%
    dplyr::select(common_name, observation_count, sampling_event_identifier, POSTALCODE, MUNICIPAL) %>% 
    group_by(POSTALCODE) %>% summarise(Checklists = length(unique(sampling_event_identifier))) 

  Checklists
  })

Checklists<-do.call("rbind", Checklists)

#Join up checklists and CANMARG
CANMARGCHECKLISTS<-inner_join(CANMARG, Checklists, by="POSTALCODE")%>% 
filter(cmg06_10 > -9999)%>%
  filter(Checklists < 10000)

#linear model
m1<-glm(Checklists~cmg06_10+cmg06_11+cmg06_12+cmg06_13, data=CANMARGCHECKLISTS, family = poisson)
summary(m1)
rsq(m1)

#More instability = more checklists - weak relationship
#Less depravation = more checklists - weak relationship
#More dependency = more checklists
#less ethnic concentration = more checklists

plot(Checklists~ cmg06_10, data=CANMARGCHECKLISTS)
plot(Checklists~ cmg06_11, data=CANMARGCHECKLISTS)
plot(Checklists~ cmg06_12, data=CANMARGCHECKLISTS)
plot(Checklists~ cmg06_13, data=CANMARGCHECKLISTS)

