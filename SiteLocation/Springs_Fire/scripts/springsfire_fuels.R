
library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
getwd()


Pre_SpringsFire_FUELS <- read_csv("Springs_Fire/data/raw/pre_springsfire_finefuels.csv")
Pre_SpringsFire_FUELS
SpringsFire_treatment <- read_csv("Springs_Fire/data/raw/springsfire_treatment.csv")

##Browns equations
                              # Sq diam        specific grav   angle corr fact   length of line (ft)
                              # 1 hr: 0.0151     0.48            1.13             2*3.28
                              # 10 hr: 0.289    0.48            1.13              2*3.28
                              # 100 hr: 2.76   0.40            1.13               4*3.28
                              # cwd sound       0.40            1.00              11.3*3.28
                              # cwd rotten      0.30            1.00              11.3*3.28
                              
                              # slope correction = (1+(presprings_fuelslope/100)^2)^(1/2)
                              #slopecorrection= (1+(Pre_SpringsFire_FUELS$slope_percent/100)^2)^(1/2) #QAQC on error in slope
                             
                              # convert sq cm to sq in: * 0.155
                    
                    PreSpringsFuels_Slope <- Pre_SpringsFire_FUELS  %>% 
                            mutate(slopecorrection = (1+(Pre_SpringsFire_FUELS$slope_percent/100)^2)^(1/2))
                          
                    #Create New data frame
                    prespringsfuels<-NULL
                    prespringsfuels<-as.data.frame(matrix(NA, ncol=6, nrow=nrow(PreSpringsFuels_Slope)))
                    colnames(prespringsfuels)<-c("plot_id","mass_1hr", "mass_10hr", "mass_100hr", "mass_cwd_sound", "mass_cwd_rotten")
                    
                    #run a forloop to apply the equations to each row in the data frame
                    for (i in 1:nrow(PreSpringsFuels_Slope)) {
                    	      
                      prespringsfuels[i,"plot_id"]<-Pre_SpringsFire_FUELS[i,"plot_id"]          
                      prespringsfuels[i,"mass_1hr"]<- ((11.64 * PreSpringsFuels_Slope[i,"count_x1h"] * 0.0151 * 	0.48 * 1.13 * slopecorrection[i])/ (2*3.28))         
                      prespringsfuels[i,"mass_10hr"]<-((11.64 * PreSpringsFuels_Slope[i,"count_x10h"] * 0.289 * 	0.48 * 1.13 * 1) / (2*3.28))        
                      prespringsfuels[i,"mass_100hr"]<-(11.64 * PreSpringsFuels_Slope[i,"count_x100h"] * 2.76 * 	0.40 * 1.13 * 1) / (4*3.28)           
                      prespringsfuels[i,"mass_cwd_sound"]<- ((11.64 * PreSpringsFuels_Slope[i,"sum_d2_1000s_cm2"] 	* 0.155 * 0.40 * 1) / (11.3*3.28))           
                      prespringsfuels[i,"mass_cwd_rotten"]<-(11.64 * PreSpringsFuels_Slope[i,"sum_d2_1000r_cm2"] 	* 0.155 * 0.30 * 1) / (11.3*3.28)    
                            
                    }      
                       prespringsfuels
                                       
                    ##Average each mass type per plot


#Equations aggregated per plot
                       
              prespringsfuels_complete<- prespringsfuels %>% 
                            group_by(plot_id) %>% 
                            summarise_all(list(mean))

#combine with treatment

                  #check first
                    summary(prespringsfuels_complete$plot_id)
                    summary(SpringsFire_treatment)
            
            prespringsfuels_complete_joined <- left_join(prespringsfuels_complete,
                                                         SpringsFire_treatment,
                                                         by = "plot_id")
            
          
            
#plot
     

            ggplot(data=prespringsfuels_complete_joined, mapping = aes(x=treatment_type, y=mass_1hr)) +
              geom_point()

            ggplot(data=prespringsfuels_complete_joined, mapping = aes(x=treatment_type, y=mass_10hr)) +
              geom_point()
            
            ggplot(data=prespringsfuels_complete_joined, mapping = aes(x=treatment_type, y=mass_100hr)) +
              geom_point()
            
            ggplot(data=prespringsfuels_complete_joined, mapping = aes(x=treatment_type, y=mass_cwd_sound)) +
              geom_point()
            
            ggplot(data=prespringsfuels_complete_joined, mapping = aes(x=treatment_type, y=mass_cwd_rotten)) +
              geom_point()
            

                