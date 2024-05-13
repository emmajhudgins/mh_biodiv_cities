library(tidyverse)
library(patchwork)

###variable means and standard deviations for unscaling
desc_both<-read.csv('data/column_mean_sd_both_ebird.csv')
desc_high<-read.csv('data/column_mean_sd_high_ebird.csv')
desc_low<-read.csv('data/column_mean_sd_low_ebird.csv')

######### both  ##########
mod4_health<-read.csv('data/model_selection_tables/adj_mod4_mh_both_ptable_mice_linear.csv')
mod4_health$mod='Health'
mod4_health$data='both'

mod1_bio<-read.csv('data/model_selection_tables/adj_mod1_mh_both_ptable_linear.csv')
mod1_bio$mod='Biodiversity'
mod1_bio$data='both'

mod3_socio<-read.csv('data/model_selection_tables/adj_mod3_mh_both_ptable_linear.csv')
mod3_socio$mod='Socio-demographic'
mod3_socio$data='both'

######### high marginalization ##########
mod4_high<-read.csv('data/model_selection_table_28Jul23/adj_mod4_mh_high_ptable_mice_linear.csv')
mod4_high$mod='Health'
mod4_high$data='low'

mod1_high<-read.csv('data/model_selection_table_28Jul23/adj_mod1_mh_high_ptable_linear.csv')
mod1_high$mod='Biodiversity'
mod1_high$data='low'

mod3_high<-read.csv('data/model_selection_table_28Jul23/adj_mod3_mh_high_ptable_linear.csv')
mod3_high$mod='Socio-demographic'
mod3_high$data = "low"

######## low marginalization ##########

mod4_low<-read.csv('data/model_selection_table_28Jul23/adj_mod4_mh_low_ptable_mice_linear.csv')
mod4_low$mod='Health'
mod4_low$data='high'

mod1_low<-read.csv('data/model_selection_table_28Jul23/adj_mod1_mh_low_ptable_linear.csv')
mod1_low$mod='Biodiversity'
mod1_low$data = 'high'


mod3_low<-read.csv('data/model_selection_table_28Jul23/adj_mod3_mh_low_ptable_linear.csv')
mod3_low$mod='Socio-demographic'
mod3_low$data = 'high'

############# logit ##########

x1<-seq(-3,3, length.out=200)
x2<-seq(-0.5,6, length.out=200)

logitify <- function(int,slope,x = seq(-3,3, length.out=200)) {
            z<-(int+(slope)*x)
            return(exp(-1*z)/(1+exp(-1*z)))
}



########### Both Data, Tree Species #################

# Model 4, tree species richness
both_data_tree <- as.data.frame(logitify(mod4_health$Estimate[which(mod4_health$X=='(Intercept)')]
                                     ,mod4_health$Estimate[which(mod4_health$X=='treerich')], x=x2))

colnames(both_data_tree) <- c('probability_mod4')


both_data_tree$SEmax_mod4 <- logitify(mod4_health$Estimate[which(mod4_health$X=='(Intercept)')]+
                                                  mod4_health$Std..Error[which(mod4_health$X=='(Intercept)')]
                                       ,mod4_health$Estimate[which(mod4_health$X=='treerich')] +
                                                  mod4_health$Std..Error[which(mod4_health$X=='treerich')], x=x2)

both_data_tree$SEmin_mod4 <- logitify(mod4_health$Estimate[which(mod4_health$X=='(Intercept)')]-
                                                   mod4_health$Std..Error[which(mod4_health$X=='(Intercept)')]
                                       ,mod4_health$Estimate[which(mod4_health$X=='treerich')] -
                                                   mod4_health$Std..Error[which(mod4_health$X=='treerich')], x=x2)


### Model 1, tree spp ###
both_data_tree$probability_mod1 <- logitify(mod1_bio$Estimate[which(mod1_bio$X=='(Intercept)')]
                                         ,mod1_bio$Estimate[which(mod1_bio$X=='treerich')], x=x2)

both_data_tree$SEmax_mod1 <- logitify(mod1_bio$Estimate[which(mod1_bio$X=='(Intercept)')]+
                                                   mod1_bio$Std..Error[which(mod1_bio$X=='(Intercept)')]
                                       ,mod1_bio$Estimate[which(mod1_bio$X=='treerich')] +
                                                   mod1_bio$Std..Error[which(mod1_bio$X=='treerich')], x=x2)


both_data_tree$SEmin_mod1 <- logitify(mod1_bio$Estimate[which(mod1_bio$X=='(Intercept)')]-
                                                   mod1_bio$Std..Error[which(mod1_bio$X=='(Intercept)')]
                                       ,mod1_bio$Estimate[which(mod1_bio$X=='treerich')] -
                                                   mod1_bio$Std..Error[which(mod1_bio$X=='treerich')], x=x2)



#### Model 3, tree spp 
both_data_tree$probability_mod3 <- logitify(mod3_socio$Estimate[which(mod3_socio$X=='(Intercept)')]
                                            ,mod3_socio$Estimate[which(mod3_socio$X=='treerich')], x=x2)

both_data_tree$SEmax_mod3 <- logitify(mod3_socio$Estimate[which(mod3_socio$X=='(Intercept)')]+
                                                   mod3_socio$Std..Error[which(mod3_socio$X=='(Intercept)')]
                                       ,mod3_socio$Estimate[which(mod3_socio$X=='treerich')] +
                                                   mod3_socio$Std..Error[which(mod3_socio$X=='treerich')], x=x2)

both_data_tree$SEmin_mod3 <- logitify(mod3_socio$Estimate[which(mod3_socio$X=='(Intercept)')]-
                                                   mod3_socio$Std..Error[which(mod3_socio$X=='(Intercept)')]
                                       ,mod3_socio$Estimate[which(mod3_socio$X=='treerich')]-
                                                   mod3_socio$Std..Error[which(mod3_socio$X=='treerich')], x=x2)

both_data_tree$x <- x2*as.numeric(desc_both$Standard.Deviation[which(desc_both$count.26219=='treerich')])+as.numeric(desc_both$Mean[which(desc_both$count.26219=='treerich')])

bothdata_tree <- both_data_tree %>% 
            pivot_longer(
                        cols = !x, 
                        names_to = c("value","model"),
                        names_sep = "_",
                        values_to = "y"
            )


both_tree<- bothdata_tree %>% pivot_wider(names_from = value,
                                           values_from = y)


write.csv(both_tree, 'output/probability_both_data_tree.csv', row.names = FALSE)




########## Both Data, Bird Species ############

# Model 4, bird spp
both_data_bird <- as.data.frame(logitify(mod4_health$Estimate[which(mod4_health$X=='(Intercept)')]
                                         ,mod4_health$Estimate[which(mod4_health$X=='ModeledSDiv')]))

colnames(both_data_bird) <- c('probability_mod4')


both_data_bird$SEmax_mod4 <- logitify(mod4_health$Estimate[which(mod4_health$X=='(Intercept)')]+
                                                   mod4_health$Std..Error[which(mod4_health$X=='(Intercept)')]
                                       ,mod4_health$Estimate[which(mod4_health$X=='ModeledSDiv')] +
                                                   mod4_health$Std..Error[which(mod4_health$X=='ModeledSDiv')])

both_data_bird$SEmin_mod4 <- logitify(mod4_health$Estimate[which(mod4_health$X=='(Intercept)')]-
                                                   mod4_health$Std..Error[which(mod4_health$X=='(Intercept)')]
                                       ,mod4_health$Estimate[which(mod4_health$X=='ModeledSDiv')] -
                                                   mod4_health$Std..Error[which(mod4_health$X=='ModeledSDiv')])


### Model 1 ###
both_data_bird$probability_mod1 <- logitify(mod1_bio$Estimate[which(mod1_bio$X=='(Intercept)')]
                                            ,mod1_bio$Estimate[which(mod1_bio$X=='ModeledSDiv')])

both_data_bird$SEmax_mod1 <- logitify(mod1_bio$Estimate[which(mod1_bio$X=='(Intercept)')]+
                                                   mod1_bio$Std..Error[which(mod1_bio$X=='(Intercept)')]
                                       ,mod1_bio$Estimate[which(mod1_bio$X=='ModeledSDiv')] +
                                                   mod1_bio$Std..Error[which(mod1_bio$X=='ModeledSDiv')])


both_data_bird$SEmin_mod1 <- logitify(mod1_bio$Estimate[which(mod1_bio$X=='(Intercept)')]-
                                                   mod1_bio$Std..Error[which(mod1_bio$X=='(Intercept)')]
                                       ,mod1_bio$Estimate[which(mod1_bio$X=='ModeledSDiv')] -
                                                   mod1_bio$Std..Error[which(mod1_bio$X=='ModeledSDiv')])



#### Model 3 
both_data_bird$probability_mod3 <- logitify(mod3_socio$Estimate[which(mod3_socio$X=='(Intercept)')]
                                            ,mod3_socio$Estimate[which(mod3_socio$X=='ModeledSDiv')])

both_data_bird$SEmax_mod3 <- logitify(mod3_socio$Estimate[which(mod3_socio$X=='(Intercept)')]+
                                                   mod3_socio$Std..Error[which(mod3_socio$X=='(Intercept)')]
                                       ,mod3_socio$Estimate[which(mod3_socio$X=='ModeledSDiv')] +
                                                   mod3_socio$Std..Error[which(mod3_socio$X=='ModeledSDiv')])

both_data_bird$SEmin_mod3 <- logitify(mod3_socio$Estimate[which(mod3_socio$X=='(Intercept)')]-
                                                   mod3_socio$Std..Error[which(mod3_socio$X=='(Intercept)')]
                                       ,mod3_socio$Estimate[which(mod3_socio$X=='ModeledSDiv')]-
                                                   mod3_socio$Std..Error[which(mod3_socio$X=='ModeledSDiv')])



both_data_bird$x <- x1*as.numeric(desc_both$Standard.Deviation[which(desc_both$count.26219=='ModeledSDiv')])+as.numeric(desc_both$Mean[which(desc_both$count.26219=='ModeledSDiv')])


head(both_data_bird)

bothdata_bird <- both_data_bird %>% 
            pivot_longer(
                        cols = !x, 
                        names_to = c("value","model"),
                        names_sep = "_",
                        values_to = "y"
            )


both_bird <- bothdata_bird %>% pivot_wider(names_from = value,
                             values_from = y)


write.csv(both_bird, 'output/probability_both_data_bird.csv', row.names = FALSE)




########### Low marginalization, Tree richness ############
# Model 4, tree species richness
low_data_tree <- as.data.frame(logitify(mod4_low$Estimate[which(mod4_low$X=='(Intercept)')]
                                         ,mod4_low$Estimate[which(mod4_low$X=='treerich')], x=x2))

colnames(low_data_tree) <- c('probability_mod4')


low_data_tree$SEmax_mod4 <- logitify(mod4_low$Estimate[which(mod4_low$X=='(Intercept)')]+
                                                   mod4_low$Std..Error[which(mod4_low$X=='(Intercept)')]
                                       ,mod4_low$Estimate[which(mod4_low$X=='treerich')] +
                                                   mod4_low$Std..Error[which(mod4_low$X=='treerich')], x=x2)

low_data_tree$SEmin_mod4 <- logitify(mod4_low$Estimate[which(mod4_low$X=='(Intercept)')]-
                                                  mod4_low$Std..Error[which(mod4_low$X=='(Intercept)')]
                                      ,mod4_low$Estimate[which(mod4_low$X=='treerich')] -
                                                  mod4_low$Std..Error[which(mod4_low$X=='treerich')], x=x2)

### Model 1, tree spp ###
low_data_tree$probability_mod1 <- logitify(mod1_low$Estimate[which(mod1_low$X=='(Intercept)')]
                                            ,mod1_low$Estimate[which(mod1_low$X=='treerich')], x=x2)

low_data_tree$SEmax_mod1 <- logitify(mod1_low$Estimate[which(mod1_low$X=='(Intercept)')]+
                                                  mod1_low$Std..Error[which(mod1_low$X=='(Intercept)')]
                                       ,mod1_low$Estimate[which(mod1_low$X=='treerich')] +
                                                  mod1_low$Std..Error[which(mod1_low$X=='treerich')], x=x2)

low_data_tree$SEmin_mod1 <- logitify(mod1_low$Estimate[which(mod1_low$X=='(Intercept)')]-
                                                  mod1_low$Std..Error[which(mod1_low$X=='(Intercept)')]
                                      ,mod1_low$Estimate[which(mod1_low$X=='treerich')]-
                                                  mod1_low$Std..Error[which(mod1_low$X=='treerich')], x=x2)

#### Model 3, tree spp
low_data_tree$probability_mod3 <- logitify(mod3_low$Estimate[which(mod3_low$X=='(Intercept)')]
                                            ,mod3_low$Estimate[which(mod3_low$X=='treerich')], x=x2)

low_data_tree$SEmax_mod3 <- logitify(mod3_low$Estimate[which(mod3_low$X=='(Intercept)')]+
                                                  mod3_low$Std..Error[which(mod3_low$X=='(Intercept)')]
                                       ,mod3_low$Estimate[which(mod3_low$X=='treerich')] +
                                                  mod3_low$Std..Error[which(mod3_low$X=='treerich')], x=x2)

low_data_tree$SEmin_mod3 <- logitify(mod3_low$Estimate[which(mod3_low$X=='(Intercept)')]-
                                                  mod3_low$Std..Error[which(mod3_low$X=='(Intercept)')]
                                      ,mod3_low$Estimate[which(mod3_low$X=='treerich')]-
                                                  mod3_low$Std..Error[which(mod3_low$X=='treerich')], x=x2)

low_data_tree$x <- x2*as.numeric(desc_low$Standard.Deviation[which(desc_low$X=='treerich')])+as.numeric(desc_low$Mean[which(desc_low$X=='treerich')])

lowdata_tree <- low_data_tree %>% 
            pivot_longer(
                        cols = !x, 
                        names_to = c("value","model"),
                        names_sep = "_",
                        values_to = "y"
            )

low_tree <- lowdata_tree %>% pivot_wider(names_from = value,
                             values_from = y)


write.csv(low_tree, 'output/probability_low_data_tree.csv', row.names = FALSE)

colnames(low_tree)




########### Low marginalization, Bird species ############

# Model 4, bird species richness
low_data_bird <- as.data.frame(logitify(mod4_low$Estimate[which(mod4_low$X=='(Intercept)')]
                                        ,mod4_low$Estimate[which(mod4_low$X=='ModeledSDiv')]))

colnames(low_data_bird ) <- c('probability_mod4')


low_data_bird $SEmax_mod4 <- logitify(mod4_low$Estimate[which(mod4_low$X=='(Intercept)')]+
                                                 mod4_low$Std..Error[which(mod4_low$X=='(Intercept)')]
                                     ,mod4_low$Estimate[which(mod4_low$X=='ModeledSDiv')] +
                                                 mod4_low$Std..Error[which(mod4_low$X=='ModeledSDiv')])

low_data_bird $SEmin_mod4 <- logitify(mod4_low$Estimate[which(mod4_low$X=='(Intercept)')]-
                                                 mod4_low$Std..Error[which(mod4_low$X=='(Intercept)')]
                                     ,mod4_low$Estimate[which(mod4_low$X=='ModeledSDiv')] -
                                                 mod4_low$Std..Error[which(mod4_low$X=='ModeledSDiv')])

### Model 1, bird spp ###
low_data_bird$probability_mod1 <- logitify(mod1_low$Estimate[which(mod1_low$X=='(Intercept)')]
                                           ,mod1_low$Estimate[which(mod1_low$X=='ModeledSDiv')])

low_data_bird$SEmax_mod1 <- logitify(mod1_low$Estimate[which(mod1_low$X=='(Intercept)')]+
                                                 mod1_low$Std..Error[which(mod1_low$X=='(Intercept)')]
                                     ,mod1_low$Estimate[which(mod1_low$X=='ModeledSDiv')] +
                                                 mod1_low$Std..Error[which(mod1_low$X=='ModeledSDiv')])

low_data_bird$SEmin_mod1 <- logitify(mod1_low$Estimate[which(mod1_low$X=='(Intercept)')]-
                                                 mod1_low$Std..Error[which(mod1_low$X=='(Intercept)')]
                                     ,mod1_low$Estimate[which(mod1_low$X=='ModeledSDiv')]-
                                                 mod1_low$Std..Error[which(mod1_low$X=='ModeledSDiv')])

#### Model 3, bird spp
low_data_bird$probability_mod3 <- logitify(mod3_low$Estimate[which(mod3_low$X=='(Intercept)')]
                                           ,mod3_low$Estimate[which(mod3_low$X=='ModeledSDiv')])

low_data_bird$SEmax_mod3 <- logitify(mod3_low$Estimate[which(mod3_low$X=='(Intercept)')]+
                                                 mod3_low$Std..Error[which(mod3_low$X=='(Intercept)')]
                                     ,mod3_low$Estimate[which(mod3_low$X=='ModeledSDiv')] +
                                                 mod3_low$Std..Error[which(mod3_low$X=='ModeledSDiv')])

low_data_bird$SEmin_mod3 <- logitify(mod3_low$Estimate[which(mod3_low$X=='(Intercept)')]-
                                                 mod3_low$Std..Error[which(mod3_low$X=='(Intercept)')]
                                     ,mod3_low$Estimate[which(mod3_low$X=='ModeledSDiv')]-
                                                 mod3_low$Std..Error[which(mod3_low$X=='ModeledSDiv')])

low_data_bird$x <-x1*as.numeric(desc_low$Standard.Deviation[which(desc_low$X=='ModeledSDiv')])+as.numeric(desc_low$Mean[which(desc_low$X=='ModeledSDiv')])

lowdata_bird <- low_data_bird %>% 
            pivot_longer(
                        cols = !x, 
                        names_to = c("value","model"),
                        names_sep = "_",
                        values_to = "y"
            )

low_bird <- lowdata_bird %>% pivot_wider(names_from = value,
                                         values_from = y)


write.csv(low_bird, 'output/probability_low_data_bird.csv', row.names = FALSE)






######### High marginalization, Tree richness ############
# Model 4, bird species richness
high_data_tree <- as.data.frame(logitify(mod4_high$Estimate[which(mod4_high$X=='(Intercept)')]
                                        ,mod4_high$Estimate[which(mod4_high$X=='treerich')], x=x2))

colnames(high_data_tree) <- c('probability_mod4')


high_data_tree$SEmax_mod4 <- logitify(mod4_high$Estimate[which(mod4_high$X=='(Intercept)')]+
                                                  mod4_high$Std..Error[which(mod4_high$X=='(Intercept)')]
                                     ,mod4_high$Estimate[which(mod4_high$X=='treerich')] +
                                                 mod4_high$Std..Error[which(mod4_high$X=='treerich')], x=x2)

high_data_tree$SEmin_mod4 <- logitify(mod4_high$Estimate[which(mod4_high$X=='(Intercept)')]-
                                                  mod4_high$Std..Error[which(mod4_high$X=='(Intercept)')]
                                     ,mod4_high$Estimate[which(mod4_high$X=='treerich')] -
                                                 mod4_high$Std..Error[which(mod4_high$X=='treerich')], x=x2)

### Model 1, bird spp ###
high_data_tree$probability_mod1 <- logitify(mod1_high$Estimate[which(mod1_high$X=='(Intercept)')]
                                           ,mod1_high$Estimate[which(mod1_high$X=='treerich')], x=x2)

high_data_tree$SEmax_mod1 <- logitify(mod1_high$Estimate[which(mod1_high$X=='(Intercept)')]+
                                                 mod1_high$Std..Error[which(mod1_high$X=='(Intercept)')]
                                     ,mod1_high$Estimate[which(mod1_high$X=='treerich')] +
                                                 mod1_high$Std..Error[which(mod1_high$X=='treerich')], x=x2)

high_data_tree$SEmin_mod1 <- logitify(mod1_high$Estimate[which(mod1_high$X=='(Intercept)')]-
                                                 mod1_high$Std..Error[which(mod1_high$X=='(Intercept)')]
                                     ,mod1_high$Estimate[which(mod1_high$X=='treerich')]-
                                                 mod1_high$Std..Error[which(mod1_high$X=='treerich')], x=x2)

#### Model 3, tree spp
high_data_tree$probability_mod3 <- logitify(mod3_high$Estimate[which(mod3_high$X=='(Intercept)')]
                                           ,mod3_high$Estimate[which(mod3_high$X=='treerich')], x=x2)

high_data_tree$SEmax_mod3 <- logitify(mod3_high$Estimate[which(mod3_high$X=='(Intercept)')]+
                                                 mod3_high$Std..Error[which(mod3_high$X=='(Intercept)')]
                                     ,mod3_high$Estimate[which(mod3_high$X=='treerich')] +
                                                 mod3_high$Std..Error[which(mod3_high$X=='treerich')], x=x2)

high_data_tree$SEmin_mod3 <- logitify(mod3_high$Estimate[which(mod3_high$X=='(Intercept)')]-
                                                 mod3_high$Std..Error[which(mod3_high$X=='(Intercept)')]
                                     ,mod3_high$Estimate[which(mod3_high$X=='treerich')]-
                                                 mod3_high$Std..Error[which(mod3_high$X=='treerich')], x=x2)

high_data_tree$x <-  x2*as.numeric(desc_high$Standard.Deviation[which(desc_high$X=='treerich')])+as.numeric(desc_high$Mean[which(desc_high$X=='treerich')])

highdata_tree <- high_data_tree %>% 
            pivot_longer(
                        cols = !x, 
                        names_to = c("value","model"),
                        names_sep = "_",
                        values_to = "y"
            )

high_tree <- highdata_tree %>% pivot_wider(names_from = value,
                                         values_from = y)


write.csv(high_tree, 'output/probability_high_data_tree.csv', row.names = FALSE)










######### High marginalization, Bird spp richness ############

# Model 4, bird species richness
high_data_bird <- as.data.frame(logitify(mod4_high$Estimate[which(mod4_high$X=='(Intercept)')]
                                        ,mod4_high$Estimate[which(mod4_high$X=='ModeledSDiv')]))

colnames(high_data_bird) <- c('probability_mod4')


high_data_bird$SEmax_mod4 <- logitify(mod4_high$Estimate[which(mod4_high$X=='(Intercept)')]+
                                                  mod4_high$Std..Error[which(mod4_high$X=='(Intercept)')]
                                      ,mod4_high$Estimate[which(mod4_high$X=='ModeledSDiv')] +
                                                  mod4_high$Std..Error[which(mod4_high$X=='ModeledSDiv')])

high_data_bird$SEmin_mod4 <- logitify(mod4_high$Estimate[which(mod4_high$X=='(Intercept)')]-
                                                  mod4_high$Std..Error[which(mod4_high$X=='(Intercept)')]
                                      ,mod4_high$Estimate[which(mod4_high$X=='ModeledSDiv')] -
                                                  mod4_high$Std..Error[which(mod4_high$X=='ModeledSDiv')])

plot(logitify(mod4_high$Estimate[which(mod4_high$X=='(Intercept)')]
              ,mod4_high$Estimate[which(mod4_high$X=='ModeledSDiv')]))




### Model 1, bird spp ###
high_data_bird$probability_mod1 <- logitify(mod1_high$Estimate[which(mod1_high$X=='(Intercept)')]
                                           ,mod1_high$Estimate[which(mod1_high$X=='ModeledSDiv')])

high_data_bird$SEmax_mod1 <- logitify(mod1_high$Estimate[which(mod1_high$X=='(Intercept)')]+
                                                  mod1_high$Std..Error[which(mod1_high$X=='(Intercept)')]
                                     ,mod1_high$Estimate[which(mod1_high$X=='ModeledSDiv')] +
                                                 mod1_high$Std..Error[which(mod1_high$X=='ModeledSDiv')])

high_data_bird$SEmin_mod1 <- logitify(mod1_high$Estimate[which(mod1_high$X=='(Intercept)')]-
                                                  mod1_high$Std..Error[which(mod1_high$X=='(Intercept)')]
                                     ,mod1_high$Estimate[which(mod1_high$X=='ModeledSDiv')]-
                                                 mod1_high$Std..Error[which(mod1_high$X=='ModeledSDiv')])

#### Model 3, bird spp
high_data_bird$probability_mod3 <- logitify(mod3_high$Estimate[which(mod3_high$X=='(Intercept)')]
                                           ,mod3_high$Estimate[which(mod3_high$X=='ModeledSDiv')])

high_data_bird$SEmax_mod3 <- logitify(mod3_high$Estimate[which(mod3_high$X=='(Intercept)')]+
                                                  mod3_high$Std..Error[which(mod3_high$X=='(Intercept)')]
                                     ,mod3_high$Estimate[which(mod3_high$X=='ModeledSDiv')] +
                                                 mod3_high$Std..Error[which(mod3_high$X=='ModeledSDiv')])

high_data_bird$SEmin_mod3 <- logitify(mod3_high$Estimate[which(mod3_high$X=='(Intercept)')]-
                                                  mod3_high$Std..Error[which(mod3_high$X=='(Intercept)')]
                                     ,mod3_high$Estimate[which(mod3_high$X=='ModeledSDiv')]-
                                                 mod3_high$Std..Error[which(mod3_high$X=='ModeledSDiv')])

high_data_bird$x <-  x1*as.numeric(desc_low$Standard.Deviation[which(desc_low$X=='ModeledSDiv')])+as.numeric(desc_low$Mean[which(desc_low$X=='ModeledSDiv')])

highdata_bird <- high_data_bird %>% 
            pivot_longer(
                        cols = !x, 
                        names_to = c("value","model"),
                        names_sep = "_",
                        values_to = "y"
            )

high_bird <- highdata_bird %>% pivot_wider(names_from = value,
                                         values_from = y)


write.csv(high_bird, 'output/probability_high_data_bird.csv', row.names = FALSE)




######## Make the figures ###########

both_tree <- read.csv('output/probability_both_data_tree.csv')
both_bird <- read.csv('output/probability_both_data_bird.csv')

low_tree <- read.csv('output/probability_low_data_tree.csv')
low_bird <- read.csv('output/probability_low_data_bird.csv')

high_tree <- read.csv('output/probability_high_data_tree.csv')
high_bird <- read.csv('output/probability_high_data_bird.csv')



# Make the plot

colour_label <- c("mod1" = "#97e196",
                  "mod3" = "#4c9b82",
                  "mod4" = "#105965")


colour_line <- c("mod1" = "black",
                  "mod3" = "#323031",
                  "mod4" = "#00171f")


both_tree_plot <- ggplot(both_tree, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                                      fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) + 
            ylab("Probability of good mental health") +
            xlab("Tree Richness") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none") + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line)+
            ggtitle ("All Data")+scale_x_continuous(limits=c(0,50))

both_bird_plot <- ggplot(both_bird, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                                        fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) + 
            ylab("Probability of good mental health") +
            xlab("Bird Diversity") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none") + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line)



low_tree_plot <- ggplot(low_tree, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                                      fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) + 
            ylab("Probability of good mental health") +
            xlab("Tree Richness") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none") + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line) +
            ggtitle ("Low Marginalization")+scale_x_continuous(limits=c(0,50))


low_bird_plot <- ggplot(low_bird, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                     fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) +
            ylab("Probability of good mental health") +
            xlab("Bird Diversity") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none") + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line)


high_tree_plot <- ggplot(high_tree, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                                        fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) + 
            ylab("Probability of good mental health") +
            xlab("Tree Richness") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none") + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line) +
            ggtitle ("High Marginalization")+scale_x_continuous(limits=c(0,50))

high_bird_plot <- ggplot(high_bird, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                                        fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) + 
            ylab("Probability of goox mental health") +
            xlab("Bird Diversity") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10)) + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line)


########### panel ######
library(ggpubr)

leg <- get_legend(high_bird_plot)
leg_plot <- as_ggplot(leg)

library(patchwork)

all_panel <- both_tree_plot + both_bird_plot +
            low_tree_plot + low_bird_plot + 
            high_tree_plot + high_bird_plot +
            plot_layout(ncol = 2) +
            plot_annotation(tag_levels = "A")

ggsave('output/all_panel_probabilities.jpg')
ggsave('output/all_panel_probabilities.pdf')
ggsave('output/all_panel_probabilities.eps', device=cairo_ps)


########## facet wraps ###########

both_tree_plot_fw <- ggplot(both_tree, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                                        fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) + 
            facet_wrap(~model) +
            ylab("Probability of good mental health") +
            xlab("Tree Richness") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none") + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line)+
            ggtitle ("All Data")+scale_x_continuous(limits=c(0,50))

both_bird_plot_fw <- ggplot(both_bird, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                                        fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) + 
            facet_wrap(~model) +
            ylab("Probability of good mental health") +
            xlab("Bird Diversity") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none") + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line)



low_tree_plot_fw <- ggplot(low_tree, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                                      fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) + 
            facet_wrap(~model) + 
            ylab("Probability of good mental health") +
            xlab("Tree Richness") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none") + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line) +
            ggtitle ("Low Marginalization")+scale_x_continuous(limits=c(0,50))


low_bird_plot_fw <- ggplot(low_bird, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                                      fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) + 
            facet_wrap(~model) +
            ylab("Probability of good mental health") +
            xlab("Bird Diversity") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none") + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line)


high_tree_plot_fw <- ggplot(high_tree, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                                        fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) + 
            facet_wrap(~model) + 
            ylab("Probability of good mental health") +
            xlab("Tree Richness") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none") + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line) +
            ggtitle ("High Marginalization")+
          scale_x_continuous(limits=c(0,50))

high_bird_plot_fw <- ggplot(high_bird, aes(x=x, y=probability, ymin=SEmin, ymax=SEmax, 
                                        fill=model, linetype=model, colour = model)) + 
            geom_ribbon(alpha = 0.6, size = 0.5) + 
            geom_line(lwd = 1.5) + 
            facet_wrap(~model) + 
            ylab("Probability of good mental health") +
            xlab("Bird Diversity") +
            theme_light()+
            theme(axis.title=element_text(size=10),
                  axis.text=element_text(size=10),
                  legend.position = "none") + 
            scale_fill_manual(values = colour_label) +
            scale_colour_manual(values = colour_line)

panel_facetwrap <- both_tree_plot_fw + both_bird_plot_fw +
            low_tree_plot_fw + low_bird_plot_fw + 
            high_tree_plot_fw + high_bird_plot_fw +
            plot_layout(ncol = 2) +
            plot_annotation(tag_levels = "A")

ggsave('output/all_panel_probabilities_facetwrap.jpg')
ggsave('output/all_panel_probabilities_facetwrap.pdf')
ggsave('output/all_panel_probabilities_facetwrap.eps', device=cairo_ps)



