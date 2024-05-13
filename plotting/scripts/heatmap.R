library(tidyverse)
library(viridis)
library(stringr)                   # Load stringr


####### examples ########
# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# Heatmap 
ggplot(data, aes(X, Y, fill= Z)) + 
            geom_tile()




dat <- matrix(rnorm(100, 3, 1), ncol = 10)
## the matrix needs names
names(dat) <- paste("X", 1:10)

## convert to tibble, add row identifier, and shape "long"
dat2 <-
            dat %>%
            as_tibble() %>%
            rownames_to_column("Var1") %>%
            pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
            mutate(
                        Var1 = factor(Var1, levels = 1:10),
                        Var2 = factor(gsub("V", "", Var2), levels = 1:10)
            )
#> Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
#> `.name_repair` is omitted as of tibble 2.0.0.
#> ℹ Using compatibility `.name_repair`.

ggplot(dat2, aes(Var1, Var2)) +
            geom_tile(aes(fill = value)) +
            geom_text(aes(label = round(value, 1))) +
            scale_fill_gradient(low = "white", high = "red") +
            scale_x_discrete(position = "top") 





data(mtcars)
cormat <- round(cor(mtcars), 2)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
            cormat[lower.tri(cormat)]<- NA
            return(cormat)
}

upper_tri <- get_upper_tri(cormat)

# Finished correlation matrix heatmap
library(reshape2)
melted_cormat <- melt(upper_tri, 
                      na.rm=TRUE) # Melt the correlation matrix

# Heatmap


# ggplot(ggplot(data=melted_cormat[melted_cormat$value != 1 & 
#                                              abs(melted_cormat$value) > 0.3,],
#               aes(Var2, Var1, fill=value)))+
#             eom_tile(color="white")+
#             scale_fill_gradient2(low="blue", high="red", mid="white", 
#                                  midpoint=0, limit=c(-1,1), space="Lab", 
#                                  name="Pearson\nCorrelation")+
#             ggtitle("Title")+
#             xlab("V1")+
#             ylab("V2")+
#             theme_minimal()+ 
#             theme(axis.text.x=element_text(angle=90, vjust=0.5, 
#                                            size=10, hjust=1))+
#             coord_fixed()











########### Mental health data ##############

# Heatmap Goal
                                    #Both/High/Low
            #Biodiversity (mod1) Sociodem (Mod3) Health (mod4)
# Birds(sh)
# Trees(rich)
# NDVI(500)
# distBlue
# distGreen
# prop.green
# prop.blue

#Colours
#"#008080","#70a494","#b4c8a8","#f6edbd","#edbb8a","#de8a5a","#ca562c"

######## Both Data ########

mod4_health<-read.csv('data/model_selection_tables/adj_mod4_mh_both_ptable_mice_linear.csv')
mod4_health$mod='Health'

mod1_bio<-read.csv('data/model_selection_tables/adj_mod1_mh_both_ptable_linear.csv')
mod1_bio$mod='Biodiversity'

mod3_socio<-read.csv('data/model_selection_tables/adj_mod3_mh_both_ptable_linear.csv')
mod3_socio$mod='Socio-demographic'


mods_together <- as.data.frame(bind_rows(mod1_bio, mod3_socio, mod4_health))
str(mods_together)


mods_together$X <- recode(mods_together$X, `(Intercept)`='Intercept',eexp="Weekly activity time",SMKC_102='Has not quit smoking',SMKC_103='Unknown smoking cessation status',SMKC_106='Never smoked', SMKC_2022='Occasional smoker',SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency', ALCEDWKY='Weekly alcohol consumption',
                FVCDVTOT='Daily fruit and vegetable consumption', married2='Common-law', married3='Never Married', married4='Separated',married5='Divorced',married6='Widowed', married7='Unknown marital status', job1='Employed',job2='Unknown employment status', white1='White', white2='Unknown ethnicity',imi2='Non-immigrant (non-white)', 
                imi3='Immigrant (White, <10 years)',imi7='Unknown immigration status', INCDHH='Household income', EHG2DVR32='High school education', EHG2DVR33='Post-secondary education',EHG2DVR34='Unknown Education status',DHHE_SEX2='Female',DHH_AGE='Age', treerich='Tree species richness', treediv='Tree Shannon diversity', DistancetoLocation='Distance to nearest ebird hotspot', ModeledSDiv='Modeled bird Shannon diversity', ModeledSRich='Modeled bird species richness', dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity', ndvi='Greenness in postalcode (NDVI)',ndvi500='Greenness within 500m buffer (NDVI)', ndvi1000='Greenness within 1000m buffer (NDVI)',YEAR='Year',bluedist='Distance to blue space',
                greendist='Distance to green space',PropBlue="Proportion of blue space", PropGreen='Proportion of green space', area_m='Postal code area')

mods_together$group <- mods_together$X

mods_together$group <- recode(mods_together$group, 
                         "Intercept" = "NA",
                         "Weekly activity time" = "Health",
                         "Has not quit smoking" ="Health",                 
                         "Unknown smoking cessation status" = "Health",
                         "Never smoked" = "Health",
                         "Occasional smoker" = "Health",
                         "Unknown smoking frequency" = "Health",
                         "Weekly alcohol consumption" = "Health",
                         "Daily fruit and vegetable consumption" = "Health",
                         "Common-law" = "Socio-demographic" ,
                         "Never Married" = "Socio-demographic",
                         "Separated" = "Socio-demographic",
                         "Divorced" = "Socio-demographic",
                         "Widowed" = "Socio-demographic",
                         "Unknown marital status" = "Socio-demographic",
                         "Employed" = "Socio-demographic",
                         "Unknown employment status" = "Socio-demographic",           
                         "White" = "Socio-demographic",
                         "Unknown ethnicity" = "Socio-demographic",
                         "Non-immigrant (non-white)" = "Socio-demographic",            
                         "Unknown immigration status" = "Socio-demographic",
                         "Household income" = "Socio-demographic",
                         "High school education" = "Socio-demographic",                
                         "Post-secondary education" = "Socio-demographic",
                         "Unknown Education status" = "Socio-demographic",
                         "Female" = "Socio-demographic",                               
                         "Age" = "Socio-demographic",
                         "Tree species richness" = "Biodiversity",
                         "Distance to nearest ebird hotspot" = "Biodiversity",    
                         "Modeled bird Shannon diversity" = "Biodiversity",
                         "Greenness within 500m buffer (NDVI)" = "Biodiversity",
                         "Year" = "NA",                                
                         "Distance to blue space" = "Biodiversity",
                         "Distance to green space" = "Biodiversity",
                         "Proportion of blue space" = "Biodiversity",             
                         "Proportion of green space" = "Biodiversity",
                         "Postal code area" = "Socio-demographic",
                         "Non-smoker" = "Health")


mods_together <- rename(mods_together,
                 variable = X,
                 P = Pr...z..,
                 std.error = Std..Error)

write.csv(mods_together,"data/adj_mod134_mh_both_ptable_mice_linear_heatmap.csv")


####### Both data figures #######
mods_together  <- mods_together  %>% filter(variable != "Immigrant (White, <10 years)")

mods_together_bd <- mods_together %>% 
            filter(group == "Biodiversity")

mods_both_sig <- mods_together_bd %>% 
            filter(P < 0.05)

unique(mods_together_bd$mod)


mods_together_bd$mod <- factor(mods_together_bd$mod, levels = c('Biodiversity',
                                                                
                                                              'Socio-demographic',   'Health'))

both_heatmap <- mods_together_bd %>% 
            ggplot(aes(mod,variable, fill = -1*Estimate)) +
            geom_tile() +
            geom_text(aes(label = round(-1*Estimate, 2),
                          colour = P < 0.05),
                      size = 5) +
            scale_x_discrete(position = "top") +
            scale_fill_gradient2(low = "#fde725", mid = "white",high = "#045275") +
            theme_void() +
            theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=10),
                  plot.title = element_text(hjust = 0.5,
                                            size = 14),
                  axis.text.y = element_text(hjust = 0.99),
                  legend.position = "none")+
            xlab(" ") +
            ylab(" ") +
            scale_color_manual(values = c("lightgrey","black")) +
            ggtitle("All data")


####### Low Data #########

mod4_low<-read.csv('data/model_selection_table_28Jul23/adj_mod4_mh_low_ptable_mice_linear.csv')
mod4_low$mod='Health'

mod1_low<-read.csv('data/model_selection_table_28Jul23/adj_mod1_mh_low_ptable_linear.csv')
mod1_low$mod='Biodiversity'

mod3_low<-read.csv('data/model_selection_table_28Jul23/adj_mod3_mh_low_ptable_linear.csv')
mod3_low$mod='Socio-demographic'


mods_low <- bind_rows(mod1_low, mod3_low, mod4_low)
str(mods_low)


mods_low$X <- recode(mods_low$X, `(Intercept)`='Intercept',eexp="Weekly activity time",SMKC_102='Has not quit smoking',SMKC_103='Unknown smoking cessation status',SMKC_106='Never smoked', SMKC_2022='Occasional smoker',SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency', ALCEDWKY='Weekly alcohol consumption',
                          FVCDVTOT='Daily fruit and vegetable consumption', married2='Common-law', married3='Never Married', married4='Separated',married5='Divorced',married6='Widowed', married7='Unknown marital status', job1='Employed',job2='Unknown employment status', white1='White', white2='Unknown ethnicity',imi2='Non-immigrant (non-white)', 
                          imi3='Immigrant (White, <10 years)',imi7='Unknown immigration status', INCDHH='Household income', EHG2DVR32='High school education', EHG2DVR33='Post-secondary education',EHG2DVR34='Unknown Education status',DHHE_SEX2='Female',DHH_AGE='Age', treerich='Tree species richness', treediv='Tree Shannon diversity', 
                     DistancetoLocation='Distance to nearest ebird hotspot', ModeledSDiv='Modeled bird Shannon diversity', ModeledSRich='Modeled bird species richness', dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity', ndvi='Greenness in postalcode (NDVI)',ndvi500='Greenness within 500m buffer (NDVI)', ndvi1000='Greenness within 1000m buffer (NDVI)',YEAR='Year',bluedist='Distance to blue space',
                          greendist='Distance to green space',PropBlue="Proportion of blue space", PropGreen='Proportion of green space', area_m='Postal code area')

mods_low$group <- mods_low$X

mods_low$group <- recode(mods_low$group, 
                              "Intercept" = "NA",
                              "Weekly activity time" = "Health",
                              "Has not quit smoking" ="Health",                 
                              "Unknown smoking cessation status" = "Health",
                              "Never smoked" = "Health",
                              "Occasional smoker" = "Health",
                              "Unknown smoking frequency" = "Health",
                              "Weekly alcohol consumption" = "Health",
                              "Daily fruit and vegetable consumption" = "Health",
                              "Common-law" = "Socio-demographic" ,
                              "Never Married" = "Socio-demographic",
                              "Separated" = "Socio-demographic",
                              "Divorced" = "Socio-demographic",
                              "Widowed" = "Socio-demographic",
                              "Unknown marital status" = "Socio-demographic",
                              "Employed" = "Socio-demographic",
                              "Unknown employment status" = "Socio-demographic",           
                              "White" = "Socio-demographic",
                              "Unknown ethnicity" = "Socio-demographic",
                              "Non-immigrant (non-white)" = "Socio-demographic",            
                              "Unknown immigration status" = "Socio-demographic",
                              "Household income" = "Socio-demographic",
                              "High school education" = "Socio-demographic",                
                              "Post-secondary education" = "Socio-demographic",
                              "Unknown Education status" = "Socio-demographic",
                              "Female" = "Socio-demographic",                               
                              "Age" = "Socio-demographic",
                              "Tree species richness" = "Biodiversity",
                              "Distance to nearest ebird hotspot" = "Biodiversity",    
                              "Modeled bird Shannon diversity" = "Biodiversity",
                              "Greenness within 500m buffer (NDVI)" = "Biodiversity",
                              "Year" = "NA",                                
                              "Distance to blue space" = "Biodiversity",
                              "Distance to green space" = "Biodiversity",
                              "Proportion of blue space" = "Biodiversity",             
                              "Proportion of green space" = "Biodiversity",
                              "Postal code area" = "Socio-demographic",
                              "Non-smoker" = "Health")


mods_low <- rename(mods_low,
                        variable = X,
                        P = Pr...z..,
                        std.error = Std..Error)

# Biodiversity subset ####
mods_low  <- mods_low %>% filter(variable != "Immigrant (White, <10 years)")

mods_low_bd <- mods_low %>% 
            filter(group == "Biodiversity")

mods_low_bd$mod <- factor(mods_low_bd$mod, levels = c('Biodiversity','Socio-demographic',
                                                                'Health'
                                                                ))




mods_low_sig <- mods_low_bd %>% 
            filter(P < 0.05)

low_heatmap <- mods_low_bd %>% 
            ggplot(aes(mod, variable, fill = -1*Estimate)) +
            geom_tile() +
            geom_text(aes(label = round(-1*Estimate, 2),
                          colour = P < 0.05),
                      size = 5) +
            scale_x_discrete(position = "top") +
            #scale_x_discrete(position = "top") +
            scale_fill_gradient2(low = "#fde725", mid = "white",high = "#045275") +
            theme_void() +
            theme(plot.title = element_text(hjust = 0.5,
                                            size = 14),
                  legend.position = "none",
                        axis.text.x = element_text(size = 10),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank())+
            xlab(" ") +
            ylab(" ") +
            scale_color_manual(values = c("lightgrey", "black")) +
            ggtitle("Low marginalization")


####### High Data #########

mod4_high<-read.csv('data/model_selection_table_28Jul23/adj_mod4_mh_high_ptable_mice_linear.csv')
mod4_high$mod='Health'

mod1_high<-read.csv('data/model_selection_table_28Jul23/adj_mod1_mh_high_ptable_linear.csv')
mod1_high$mod='Biodiversity'

mod3_high<-read.csv('data/model_selection_table_28Jul23/adj_mod3_mh_high_ptable_linear.csv')
mod3_high$mod='Socio-demographic'


mods_high <- bind_rows(mod1_high, mod3_high, mod4_high)
str(mods_high)


mods_high$X <- recode(mods_high$X, `(Intercept)`='Intercept',eexp="Weekly activity time",SMKC_102='Has not quit smoking',SMKC_103='Unknown smoking cessation status',SMKC_106='Never smoked', SMKC_2022='Occasional smoker',SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency', ALCEDWKY='Weekly alcohol consumption',
                     FVCDVTOT='Daily fruit and vegetable consumption', married2='Common-law', married3='Never Married', married4='Separated',married5='Divorced',married6='Widowed', married7='Unknown marital status', job1='Employed',job2='Unknown employment status', white1='White', white2='Unknown ethnicity',imi2='Non-immigrant (non-white)', 
                     imi3='Immigrant (White, <10 years)',imi7='Unknown immigration status', INCDHH='Household income', EHG2DVR32='High school education', EHG2DVR33='Post-secondary education',EHG2DVR34='Unknown Education status',DHHE_SEX2='Female',DHH_AGE='Age', treerich='Tree species richness', treediv='Tree Shannon diversity', DistancetoLocation='Distance to nearest ebird hotspot', ModeledSDiv='Modeled bird Shannon diversity', ModeledSRich='Modeled bird species richness', dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity', ndvi='Greenness in postalcode (NDVI)',ndvi500='Greenness within 500m buffer (NDVI)', ndvi1000='Greenness within 1000m buffer (NDVI)',YEAR='Year',bluedist='Distance to blue space',
                     greendist='Distance to green space',PropBlue="Proportion of blue space", PropGreen='Proportion of green space', area_m='Postal code area')

mods_high$group <- mods_high$X

mods_high$group <- recode(mods_high$group, 
                         "Intercept" = "NA",
                         "Weekly activity time" = "Health",
                         "Has not quit smoking" ="Health",                 
                         "Unknown smoking cessation status" = "Health",
                         "Never smoked" = "Health",
                         "Occasional smoker" = "Health",
                         "Unknown smoking frequency" = "Health",
                         "Weekly alcohol consumption" = "Health",
                         "Daily fruit and vegetable consumption" = "Health",
                         "Common-law" = "Socio-demographic" ,
                         "Never Married" = "Socio-demographic",
                         "Separated" = "Socio-demographic",
                         "Divorced" = "Socio-demographic",
                         "Widowed" = "Socio-demographic",
                         "Unknown marital status" = "Socio-demographic",
                         "Employed" = "Socio-demographic",
                         "Unknown employment status" = "Socio-demographic",           
                         "White" = "Socio-demographic",
                         "Unknown ethnicity" = "Socio-demographic",
                         "Non-immigrant (non-white)" = "Socio-demographic",            
                         "Unknown immigration status" = "Socio-demographic",
                         "Household income" = "Socio-demographic",
                         "High school education" = "Socio-demographic",                
                         "Post-secondary education" = "Socio-demographic",
                         "Unknown Education status" = "Socio-demographic",
                         "Female" = "Socio-demographic",                               
                         "Age" = "Socio-demographic",
                         "Tree species richness" = "Biodiversity",
                         "Tree Shannon diversity" = "Biodiversity",
                         "Distance to nearest ebird hotspot" = "Biodiversity",    
                         "Modeled bird Shannon diversity" = "Biodiversity",
                         "Greenness within 500m buffer (NDVI)" = "Biodiversity",
                         "Year" = "NA",                                
                         "Distance to blue space" = "Biodiversity",
                         "Distance to green space" = "Biodiversity",
                         "Proportion of blue space" = "Biodiversity",             
                         "Proportion of green space" = "Biodiversity",
                         "Postal code area" = "Socio-demographic",
                         "Non-smoker" = "Health")


mods_high<- rename(mods_high,
                   variable = X,
                   P = Pr...z..,
                   std.error = Std..Error)

# Biodiversity subset

mods_high_bd <- mods_high %>% 
            filter(group == "Biodiversity")



mods_high_bd$mod <- factor(mods_high_bd$mod, levels = c('Biodiversity', 'Socio-demographic',    'Health'  ))



mods_high_sig <- mods_high_bd %>% 
            filter(P < 0.05)

high_heatmap <- mods_high_bd %>% 
            ggplot(aes(mod, variable, fill = -1*Estimate)) +
            geom_tile() +
            geom_text(aes(label = round(-1*Estimate, 2),
                          colour = P < 0.05),
                      size = 5) +
            scale_x_discrete(position = "top") +
            scale_fill_gradient2(low = "#fde725", mid = "white",high = "#045275") +
            theme_void() +
            theme(plot.title = element_text(hjust = 0.5,
                                            size = 14),
                  axis.text.x = element_text(size = 10),
                  # axis.text.y = element_blank(),
                  # axis.ticks.y = element_blank(),
                  legend.position = "none")+
            xlab(" ") +
            ylab(" ") +
            scale_color_manual(values = c("lightgrey", "black")) +
            ggtitle("High marginalization")


# order of models 'biodiversity, socio, health'
# change label to be greyed out p <0.05
#left labels be right aligned

########### panel ######
library(ggpubr)

leg <- get_legend(both_heatmap)
leg_plot <- as_ggplot(leg)

library(patchwork)

layout <- "
AAABBBCCCD
"

heatmap <- both_heatmap + low_heatmap + 
            high_heatmap + leg_plot + plot_layout(design = layout)

ggsave("output/heatmap.jpg", heatmap)

ggsave("output/heatmap.pdf", heatmap)
ggsave("output/heatmap.eps", heatmap, device=cairo_ps)
