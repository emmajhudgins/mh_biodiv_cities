
# Colours etc.

#Biodiversity - Trees, NDVI, Birds, Green space, Blue space 
"#386641"

#Health behaviours - fruit & veg consumption, smoking, smoking cessation, alcohol, physical activity
"#540b0e"

#Socio demographics - education, income, marital status, employment, sex, age, race, time since immigration, born in canada
"#003566" 



## plotting script

mod4t<-read.csv('data/model_selection_tables/adj_mod4_mh_both_ptable_mice_linear.csv')
mod4t$mod='4'

mod1t<-read.csv('data/model_selection_tables/adj_mod1_mh_both_ptable_linear.csv')
mod1t$mod='1'

mod2t<-read.csv('data/model_selection_tables/adj_mod2_mh_both_ptable_linear.csv')
mod2t$mod='2'

mod3t<-read.csv('data/model_selection_tables/adj_mod3_mh_both_ptable_linear.csv')
mod3t$mod='3'

library(dplyr)
library(ggforce) # rectangles
library(ggplot2)

modst<-bind_rows(mod1t,mod2t,mod3t,mod4t)
modst$X<-recode(modst$X, `(Intercept)`='Intercept', 
               eexp="Weekly activity time",  
               SMKC_102='Has not quit smoking',
               SMKC_104='Unknown smoking cessation status',  
               SMKC_106='Never smoked', SMKC_2022='Occasional smoker', 
               SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency',  
               ALCEDWKY='Weekly alcohol consumption',
               FVCDVTOT='Daily fruit and vegetable consumption', 
               married2='Common-law', married3='Never Married',
               married4='Separated',married5='Divorced',married6='Widowed', 
               married7='Unknown marital status', job1='Employed',
               job2='Unknown employment status',white1='White', 
               white2='Unknown ethnicity',imi2='Non-immigrant (non-white)', 
               imi3='Immigrant (White, <10 years)',imi7='Unknown immigration status',
               INCDHH='Household income',EHG2DVR32='High school education',
               EHG2DVR33='Post-secondary education',EHG2DVR34='Unknown Education status',DHHE_SEX2='Female', DHH_AGE='Age', treerich='Tree species richness',treediv='Tree Shannon diversity', DistancetoLocation='Distance to nearest ebird hotspot', ModeledSDiv='Modeled bird Shannon diversity',ModeledSRich='Modeled bird species richness', dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity',  ndvi='Greenness in postalcode (NDVI)',  ndvi500='Greenness within 500m buffer (NDVI)', ndvi1000='Greenness within 1000m buffer (NDVI)',YEAR='Year',bluedist='Distance to blue space', greendist='Distance to green space', PropBlue="Proportion of blue space", PropGreen='Proportion of green space',area_m='Postal code area')


mod4t$X<-recode(mod4t$X, `(Intercept)`='Intercept',
                eexp="Weekly activity time", 
               SMKC_102='Has not quit smoking',
               SMKC_103='Unknown smoking cessation status',  
               SMKC_106='Never smoked', SMKC_2022='Occasional smoker',
               SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency',  
               ALCEDWKY='Weekly alcohol consumption',
               FVCDVTOT='Daily fruit and vegetable consumption', 
               married2='Common-law', married3='Never Married',
               married4='Separated',married5='Divorced',
               married6='Widowed', married7='Unknown marital status', 
               job1='Employed',job2='Unknown employment status',
               white1='White', white2='Unknown ethnicity',
               imi2='Non-immigrant (non-white)', 
               imi3='Immigrant (White, <10 years)',
               imi7='Unknown immigration status',
               INCDHH='Household income',
               EHG2DVR32='High school education',
               EHG2DVR33='Post-secondary education',
               EHG2DVR34='Unknown Education status',DHHE_SEX2='Female', 
               DHH_AGE='Age', treerich='Tree species richness',
               treediv='Tree Shannon diversity', 
               DistancetoLocation='Distance to nearest ebird hotspot', 
               ModeledSDiv='Modeled bird Shannon diversity',
               ModeledSRich='Modeled bird species richness', 
               dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',
               dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity', 
               ndvi='Greenness in postalcode (NDVI)',  
               ndvi500='Greenness within 500m buffer (NDVI)', 
               ndvi1000='Greenness within 1000m buffer (NDVI)',
               YEAR='Year',bluedist='Distance to blue space', 
               greendist='Distance to green space', 
               PropBlue="Proportion of blue space", 
               PropGreen='Proportion of green space',
               area_m='Postal code area')



unique(mod4t$X)
mod4t
#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

labels <- c("NA" = "darkgrey",
            "Health" = "#ca562c",
            "Socio-demographic" = "#481a6c",
            "Biodiversity" = "#6a994e")

library(glue)
library(ggtext)

mod4t <- mod4t %>% filter(X != "Immigrant (White, <10 years)")

mod4t$variable <- mod4t$X
            
mod4t$variable <- recode(mod4t$variable, 
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

############## Main Figure #####################
or_MH_plot <- ggplot(data=mod4t, 
                   aes(x=-1*Estimate, y=reorder(X, -1*Estimate),
                       xmin=-1*(Estimate-Std..Error),
                       xmax=-1*(Estimate+Std..Error),
                       fill=Pr...z..<0.05,
                       colour = variable))+
                        geom_errorbar(lwd = 1)+
                        geom_point(size = 3, stroke = 1.5, shape = 21)+
                        scale_x_continuous(limits=c(-2,2), 
                               breaks=c(log(0.01),
                                        log(0.1),log(0.2), 
                                        log(0.5),0,log(2),log(5), 
                                        log(10),log(100)),
                               labels=c(0.01,0.1,0.2,0.5,1,2,5,10,100))+
                        theme_classic()+
                        geom_vline(xintercept=0, colour='black', 
                                   linetype='dashed',
                                   lwd = 1,
                                   alpha = 0.45)+
                        scale_fill_manual(values = c("#003052", "lightgrey"))+
                        scale_colour_manual(values = labels) +
                        xlab('Odds ratio')+
                        ylab(NULL)+
                        theme(axis.title=element_text(size=12),
                              axis.text=element_text(size=12),
                              legend.position = "top") +
                        guides(fill = guide_legend(title = "P < 0.05"),
                              shape = guide_legend(title = "P < 0.05"),
                                colour = guide_legend(title = "Category")) +
             annotate("rect", xmin = -0.25, xmax = 0.25,
                      ymin = 12.5, ymax = 25.5,
                      alpha = 0.3,
                      fill = "lightgrey")
            

ggsave("output/MH_oddsratio.jpg")


####### extras ###########
biodiversity <- mod4t %>% 
            filter(variable == "Biodiversity") %>% 
            ggplot(aes(x=-1*Estimate, y=reorder(X, -1*Estimate),
                         xmin=-1*(Estimate-Std..Error),
                         xmax=-1*(Estimate+Std..Error),
                         fill=Pr...z..<0.05,
                         colour = variable))+
            geom_errorbar(lwd = 1)+
            geom_point(size = 3, stroke = 1.5, shape = 21)+
            scale_x_continuous(limits=c(-0.5,0.5))+
                               # breaks=c(log(0.01),
                               #          log(0.1),log(0.2),
                               #          log(0.5),0,log(2),log(5),
                               #          log(10),log(100)),
                               # labels=c(0.01,0.1,0.2,0.5,1,2,5,10,100))+
            theme_light()+
            geom_vline(xintercept=0, colour='black', 
                       linetype='dashed',
                       lwd = 1,
                       alpha = 0.45)+
            scale_fill_manual(values = c("#003052", "lightgrey"))+
            scale_shape_manual(values = c(22,23)) +
            scale_colour_manual(values = labels) +
            xlab('Odds ratio')+
            ggtitle("Poor mental health")+
            ylab(NULL)+
            theme(axis.title=element_text(size=12),
                  axis.text=element_text(size=12)) +
            guides(fill = guide_legend(title = "P < 0.05"),
                   shape = guide_legend(title = "P < 0.05"),
                   colour = guide_legend(title = "Category"))


ggsave("output/MH_biodiversity_oddsratio.jpg")



sig_plot <- mod4t %>% 
            filter(Pr...z.. < 0.05) %>% 
            ggplot(aes(x=-1*Estimate, y=reorder(X, -1*Estimate),
                       xmin=-1*(Estimate-Std..Error),
                       xmax=-1*(Estimate+Std..Error),
                       fill = variable,
                       colour = variable))+
            geom_errorbar(lwd = 1)+
            geom_point(size = 3, stroke = 1.5, shape = 21,
                       colour = "black")+
            #scale_x_continuous(limits=c(-0.5,0.5))+
            # breaks=c(log(0.01),
            #          log(0.1),log(0.2),
            #          log(0.5),0,log(2),log(5),
            #          log(10),log(100)),
            # labels=c(0.01,0.1,0.2,0.5,1,2,5,10,100))+
            theme_light()+
            geom_vline(xintercept=0, colour='black', 
                       linetype='dashed',
                       lwd = 1,
                       alpha = 0.45)+
            scale_fill_manual(values = labels)+
            scale_colour_manual(values = labels) +
            xlab('Odds ratio')+
            ggtitle("Poor mental health")+
            ylab(NULL)+
            theme(axis.title=element_text(size=12),
                  axis.text=element_text(size=12)) +
            guides(colour = guide_legend(title = "Category"),
                   fill = guide_legend(title = "Category"))


ggsave("output/MH_sigp_oddsratio.jpg")

######### Biodiversity effect sizes inset ################
mod4t %>% filter(variable == "Biodiversity")

list <- c("Tree species richness", "Distance to nearest ebird hotspot", "Modeled bird Shannon diversity",
          "Greenness within 500m buffer (NDVI)", "Distance to blue space", "Distance to green space",
          "Proportion of blue space", "Proportion of green space", "Postal code area", "Weekly alcohol consumption",
          "Weekly activity time", "Unknown immigration status", "Daily fruit and vegetable consumption")


limit <- mod4t %>% filter(X %in% list) %>% 
            ggplot(aes(x=-1*Estimate, y=reorder(X, -1*Estimate),
                       xmin=-1*(Estimate-Std..Error),
                       xmax=-1*(Estimate+Std..Error),
                       fill=Pr...z..<0.05,
                       colour = variable))+
            geom_errorbar(lwd = 1)+
            geom_point(size = 2, stroke = 1.5, shape = 21)+
            scale_x_continuous(limits=c(-0.25,0.25), 
                               breaks=c(log(0.8),log(0.9),0, log(1.1),log(1.2)),
                               labels=c(0.8,0.9,1, 1.1,1.2)) +
            theme_classic()+
            geom_vline(xintercept=0, colour='black', 
                       linetype='dashed',
                       lwd = 1,
                       alpha = 0.45)+
            scale_fill_manual(values = c("#003052", "lightgrey")) +
            scale_colour_manual(values = labels) +
            xlab('Odds ratio')+
            #ggtitle("Poor mental health")+
            ylab(NULL)+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none",
                  panel.background = element_rect(fill = "#F2F2F2")) +
            guides(colour = guide_legend(title = "Category"),
                   fill = guide_legend(title = "Category")) +
            theme(plot.background = element_rect(colour = "black", 
                                                 size=1))




library(patchwork)

plot2 <- or_MH_plot + inset_element(limit,
                           left = 0.53,
                           bottom = 0.05,
                           right = 0.99,
                           top = 0.5) +
            plot_annotation(tag_levels = c("A"))
            
ggsave('output/mentalhealth_plots_inset.jpg')
ggsave('output/mentalhealth_plots_inset.eps')

ggsave('output/mentalhealth_plots_inset.pdf')

 ########## Self-reported Stress #########

mod4i<-read.csv('data/model_selection_tables/adj_mod4_stress_both_ptable_mice_linear.csv')
mod4i$mod='4'
mod1i<-read.csv('data/model_selection_tables/adj_mod1_stress_both_ptable_linear.csv')
mod1i$mod='1'
mod2i<-read.csv('data/model_selection_tables/adj_mod2_stress_both_ptable_linear.csv')
mod2i$mod='2'
mod3i<-read.csv('data/model_selection_tables/adj_mod3_stress_both_ptable_linear.csv')
mod3i$mod='3'


modsi<-bind_rows(mod1i,mod2i,mod3i,mod4i)

modsi$X<-recode(modsi$X, `(Intercept)`='Intercept', eexp="Weekly activity time",  SMKC_102='Has not quit smoking',SMKC_104='Unknown smoking cessation status',  SMKC_106='Never smoked', SMKC_2022='Occasional smoker', SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency',  ALCEDWKY='Weekly alcohol consumption',FVCDVTOT='Daily fruit and vegetable consumption', married2='Common-law', married3='Never Married',married4='Separated',married5='Divorced',married6='Widowed', married7='Unknown marital status', job1='Employed',job2='Unknown employment status',white1='White', white2='Unknown ethnicity',imi2='Non-immigrant (non-white)', imi3='Immigrant (White, <10 years)',imi7='Unknown immigration status',INCDHH='Household income',EHG2DVR32='High school education',EHG2DVR33='Post-secondary education',EHG2DVR34='Unknown Education status',DHHE_SEX2='Female', DHH_AGE='Age', treerich='Tree species richness',treediv='Tree Shannon diversity', DistancetoLocation='Distance to nearest ebird hotspot', ModeledSDiv='Modeled bird Shannon diversity',ModeledSRich='Modeled bird species richness', dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity',  ndvi='Greenness in postalcode (NDVI)',  ndvi500='Greenness within 500m buffer (NDVI)', ndvi1000='Greenness within 1000m buffer (NDVI)',YEAR='Year',bluedist='Distance to blue space', greendist='Distance to green space', PropBlue="Proportion of blue space", PropGreen='Proportion of green space',area_m='Postal code area')
mod4i$X<-recode(mod4i$X, `(Intercept)`='Intercept', eexp="Weekly activity time",  SMKC_102='Has not quit smoking',SMKC_103='Unknown smoking cessation status',  SMKC_106='Never smoked', SMKC_2022='Occasional smoker', SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency',  ALCEDWKY='Weekly alcohol consumption',FVCDVTOT='Daily fruit and vegetable consumption', married2='Common-law', married3='Never Married',married4='Separated',married5='Divorced',married6='Widowed', married7='Unknown marital status', job1='Employed',job2='Unknown employment status',white1='White', white2='Unknown ethnicity',imi2='Non-immigrant (non-white)', imi3='Immigrant (White, <10 years)',imi7='Unknown immigration status',INCDHH='Household income',EHG2DVR32='High school education',EHG2DVR33='Post-secondary education',EHG2DVR34='Unknown Education status',DHHE_SEX2='Female', DHH_AGE='Age', treerich='Tree species richness',treediv='Tree Shannon diversity', DistancetoLocation='Distance to nearest ebird hotspot', ModeledSDiv='Modeled bird Shannon diversity',ModeledSRich='Modeled bird species richness', dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity',  ndvi='Greenness in postalcode (NDVI)',  ndvi500='Greenness within 500m buffer (NDVI)', ndvi1000='Greenness within 1000m buffer (NDVI)',YEAR='Year',bluedist='Distance to blue space', greendist='Distance to green space', PropBlue="Proportion of blue space", PropGreen='Proportion of green space',area_m='Postal code area')

# removed white Immigrant, <10 years because the error bars were massive
mod4i <- mod4i %>% filter(X != "Immigrant (White, <10 years)")

mod4i$variable <- mod4i$X
mod4i$variable <- recode(mod4t$variable, 
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
                         "Non-smoker" = " Health")


unique(mod4i$variable)

or_stress_plot <- ggplot(data=mod4i, 
       aes(x=-1*Estimate, y=reorder(X, -1*Estimate), 
           xmin=-1*(Estimate-Std..Error),
           xmax=-1*(Estimate+Std..Error), 
           fill=Pr...z..<0.05,
           shape=Pr...z..<0.05,
           colour = variable))+
            theme_light()+
            geom_errorbar(lwd = 1) +
            geom_point(size = 3, stroke = 1.5) +
             scale_x_continuous(limits=c(-2,2), 
                               breaks=c(log(0.1),log(0.2), 
                                        log(0.5),0,log(2),log(5), 
                                        log(10), log(100)),
                                labels=c(0.1,0.2,0.5,1,2,5,10,100))+
            geom_vline(xintercept=0, colour='black', 
                       linetype='dashed',
                       lwd = 1,
                       alpha = 0.45)+
            scale_fill_manual(values = c("black", "lightgrey"))+
            scale_shape_manual(values = c(22,23)) +
            scale_colour_manual(values = labels) +
            xlab('Odds ratio')+
            ggtitle('High perceived life stress') +
            ylab(NULL)+
            theme(axis.title=element_text(size=14),
                               axis.text=element_text(size=12)) +
            theme(axis.title=element_text(size=12),
                  axis.text=element_text(size=12)) +
            guides(fill = guide_legend(title = "P < 0.05"),
                   shape = guide_legend(title = "P < 0.05"),
                   colour = guide_legend(title = "Category"))

ggsave("output/OR_stress.jpg")
   



odds_ratio_plot <- or_MH_plot + or_stress_plot + 
            plot_annotation(tag_levels = c("A"))
            
ggsave("output/MH_stress_OR_plot.tiff")
