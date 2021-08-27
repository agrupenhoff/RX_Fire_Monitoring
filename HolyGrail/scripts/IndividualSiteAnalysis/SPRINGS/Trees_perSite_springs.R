
library(tidyverse)
library(tibble)
library(dplyr)
library(rio)
library(lme4)
library(wesanderson)
library(gtsummary)


HG_Trees_totalPlot <- read.csv("HolyGrail/data/clean/HG_Trees_totalPlot.csv")
HG_Trees_diamclass <- read.csv("HolyGrail/data/clean/HG_Trees_diamclass.csv")
HG_Trees_diamclass_species <- read.csv("HolyGrail/data/clean/HG_Trees_diamclass_species.csv")
HG_Trees_plot_species <- read.csv("HolyGrail/data/clean/HG_Trees_plot_species.csv")
NRV_diamclass <- read.csv("HolyGrail/data/clean/NRV_diamclass_BIN_youngetal.csv")
trt_utm <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_trt_utm.csv")

#############FILTER SITE & CLEAN HER UP
########

#Filter out site that you are interested in HERE#########
        springs_trees_totalPlot <- HG_Trees_totalPlot %>% 
              filter(site == "springsfire")
        springs_trees_diamclass <- HG_Trees_diamclass %>% 
              filter(site == "springsfire")
        springs_trees_plot_species <- HG_Trees_plot_species %>% 
              filter(site == "springsfire")
        springs_trees_diamclass_species <- HG_Trees_diamclass_species %>% 
                filter(site == "springsfire")
        
##Add burn unit HERE
        springs_trt <- trt_utm %>% 
                filter(site == "springsfire") %>% 
                select(plotid, TSLF, burn_unit, burn)
        springs_trees_totalPlot <- left_join(springs_trees_totalPlot, springs_trt,
                                             by="plotid")
        springs_trees_diamclass <- left_join(springs_trees_diamclass, springs_trt,
                                             by="plotid")
        springs_trees_plot_species <- left_join(springs_trees_plot_species, springs_trt,
                                             by="plotid")
        springs_trees_diamclass_species <- left_join(springs_trees_diamclass_species, springs_trt,
                                             by="plotid")

       
       
##################BY WHOLE DIAM CLASS      
        
#match springs to NRV
        NRV_diamclass_YPMC <- NRV_diamclass %>% 
          rename(diamclass = diam_class) %>% 
          distinct() %>%  #justs gives unique values for diamclass & plotTYpe
          mutate(NRV = 'range')
        
        
        springs_trees_NRV_clean <- springs_trees_diamclass %>% 
          filter(status == "LIVE") %>%
          filter(burn == "yes") %>%                 
          mutate(diamclass = factor(diamclass, levels=c('[<20]',
                                                        '[20-40]',
                                                        '[40-60]',
                                                        '[60-80]',
                                                        '[80-100]',
                                                        '[>100]'))) %>% 
          mutate(pre_post_fire = factor(pre_post_fire, levels = c("prefire",
                                                                  "postfire")))
        
        #CREATE SUMMARY 
        
        springs_diamclass_TPA <- springs_trees_NRV_clean %>%
                filter(status == "LIVE") %>% 
                group_by(pre_post_fire, diamclass) %>% 
                summarise(n_trees_ha = mean(n_trees_ha),
                          BA_m2_ha = mean(BA_m2_ha)) 
        springs_diamclass_TPA
        
        #Create PLOT
        springs_diamclass_plot <- ggplot(data=springs_trees_NRV_clean)+
          geom_boxplot(aes(x=diamclass, y=n_trees_ha, fill=pre_post_fire))+
          geom_errorbar(data= NRV_diamclass_YPMC,
                        aes(x=diamclass, ymin= NRV_lower, ymax=NRV_upper,
                            group = diamclass,
                            color= NRV), #link to group in NRV dataframe
                        width=0.2,
                        size = 1.5)+
          facet_wrap(~ site, scales = "free_y") +
          theme_minimal()+
          scale_fill_manual(values=wes_palette("BottleRocket1"))+
          theme(axis.title=element_text(size=14,face="bold"),
                axis.text=element_text(size=10))+
          xlab("Diameter Class")+
          ylab("# Trees / Ha")
        springs_diamclass_plot
        

######## BY DIAMCLASS PER SPECIES
        
        diamclass_spp <- springs_trees_diamclass_species %>% 
                filter(status == "LIVE") %>% 
                filter(burn == "yes") %>% 
                mutate(diamclass = factor(diamclass, levels=c('[<20]',
                                                              '[20-40]',
                                                              '[40-60]',
                                                              '[60-80]',
                                                              '[80-100]',
                                                              '[>100]'))) %>% 
                mutate(pre_post_fire = factor(pre_post_fire, levels = c("prefire",
                                                                        "postfire")))
        
        #CREATE SUMMARY 
        
        diamclass_spp_sum <- diamclass_spp %>%
                filter(status == "LIVE") %>% 
                group_by(pre_post_fire, diamclass, species) %>% 
                summarise(n_trees_ha = mean(n_trees_ha),
                          BA_m2_ha = mean(BA_m2_ha)) 
        diamclass_spp_sum
        
        #Create PLOT
        springs_diamclass_plot2 <- ggplot(data=diamclass_spp)+
                geom_boxplot(aes(x=diamclass, y=n_trees_ha, fill=pre_post_fire))+
                geom_errorbar(data= NRV_diamclass_YPMC,
                              aes(x=diamclass, ymin= NRV_lower, ymax=NRV_upper,
                                  group = diamclass,
                                  color= NRV), #link to group in NRV dataframe
                              width=0.2,
                              size = 1.5)+
                facet_wrap(~ species, scales = "free_y") +
                theme_minimal()+
                scale_fill_manual(values=wes_palette("BottleRocket1"))+
                theme(axis.title=element_text(size=14,face="bold"),
                      axis.text=element_text(size=10))+
                xlab("Diameter Class")+
                ylab("# Trees / Ha")
        springs_diamclass_plot2
        
        
        springs_BAspp_live <- Springs_Trees_plot_species %>% 
          filter(status == "LIVE") %>% 
          mutate(pre_post_fire = factor(pre_post_fire, levels = c("prefire",
                                                                  "postfire"))) %>% 
          filter(species == "PICO"|
                   species =="PIJE")
        springs_BAspp_plot <- ggplot(data=springs_BAspp_live)+
          geom_boxplot(aes(x=species, y=BA_m2_ha, fill=pre_post_fire))+
          facet_wrap(~ status, scales = "free_y") +
          theme_minimal()+
          scale_fill_manual(values=wes_palette("BottleRocket1"))+
          theme(axis.title=element_text(size=14,face="bold"),
                axis.text=element_text(size=10))+
          xlab("Species")+
          ylab("Basal Area (m2/acre)")
        springs_BAspp_plot
        

##BY PRIOR TREATMENT
        springs_diamclasstrt <- springs_trees_NRV_clean %>% 
                filter(burn=="yes")
        springs_diamclasstrt_plot <- ggplot(data=springs_diamclasstrt)+
                geom_boxplot(aes(x=diamclass, y=n_trees_ha, fill=pre_post_fire))+
                geom_errorbar(data= NRV_diamclass_YPMC,
                              aes(x=diamclass, ymin= NRV_lower, ymax=NRV_upper,
                                  group = diamclass,
                                  color= NRV), #link to group in NRV dataframe
                              width=0.2,
                              size = 1.5)+
                facet_wrap(~ TSLF, scales = "free_y") +
                theme_minimal()+
                scale_fill_manual(values=wes_palette("BottleRocket1"))+
                theme(axis.title=element_text(size=14,face="bold"),
                      axis.text=element_text(size=10))+
                xlab("Diameter Class")+
                ylab("# Trees / Ha")
        springs_diamclasstrt_plot

        
        ############################
##COMPARE POST FIRE STRUCTURE BETWEEN BURNED AND UNBURNED PLOTS

        #select burned plots post fire & unburned control plots to compare
        springs_trees_control <- springs_trees_diamclass %>% 
                filter(status == "LIVE") %>%
                filter(burn =="yes" & pre_post_fire=="postfire"| burn =="no") %>%                  
                mutate(diamclass = factor(diamclass, levels=c('[<20]',
                                                              '[20-40]',
                                                              '[40-60]',
                                                              '[60-80]',
                                                              '[80-100]',
                                                              '[>100]'))) %>% 
                mutate(pre_post_fire = factor(pre_post_fire, levels = c("prefire",
                                                                        "postfire")))
        
        springs_trees_control2 <- springs_trees_plot_species %>% 
                filter(status == "LIVE") %>%
                filter(burn =="yes" & pre_post_fire=="postfire"| burn =="no") 
        
        #CREATE SUMMARY 
        
        trees_control <- springs_trees_control2 %>%
                group_by(pre_post_fire, burn) %>% 
                summarise(n_trees_ha = mean(n_trees_ha),
                          BA_m2_ha = mean(BA_m2_ha)) 
        trees_control
        
        #Create PLOT
        diamclass_control_plot <- ggplot(data=springs_trees_control2)+
                geom_boxplot(aes(x=burn, y=n_trees_ha, fill=burn))+
                theme_minimal()+
                scale_fill_manual(values=wes_palette("BottleRocket1"))+
                theme(axis.title=element_text(size=14,face="bold"),
                      axis.text=element_text(size=10))+
                xlab("Burned by Springs Fire")+
                ylab("# Trees / Ha")
       diamclass_control_plot
       
       diamclass_control_plot2 <- ggplot(data=springs_trees_control2)+
               geom_boxplot(aes(x=burn, y=BA_m2_ha, fill=burn))+
               theme_minimal()+
               scale_fill_manual(values=wes_palette("BottleRocket1"))+
               theme(axis.title=element_text(size=14,face="bold"),
                     axis.text=element_text(size=10))+
               xlab("Burned by Springs Fire")+
               ylab("# Basal Area m2 / Ha")
       diamclass_control_plot2
