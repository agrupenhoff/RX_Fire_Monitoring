
library(tidyverse)
library(tibble)
library(dplyr)
library(rio)
library(lme4)
library(wesanderson)
library(gtsummary)
library(ggpubr)


HG_Trees_final <- read.csv("HolyGrail/data/clean/HG_Trees_final.csv")
HG_Trees_totalPlot <- read.csv("HolyGrail/data/clean/HG_Trees_totalPlot.csv")
HG_Trees_diamclass <- read.csv("HolyGrail/data/clean/HG_Trees_diamclass_SUM.csv")
HG_Trees_plot_species <- read.csv("HolyGrail/data/clean/HG_Trees_plot_species.csv")
NRV_diamclass <- read.csv("HolyGrail/data/clean/NRV_diamclass_BIN_youngetal.csv")
HG_trees_severityIndicies <- read.csv("HolyGrail/data/clean/HG_Trees_severityIndicies.csv") 
trt_utm <- read.csv("HolyGrail/data/raw/CPFMP_HolyGrail_trt_utm.csv")

str(trt_utm$ï..site)
unique(trt_utm$ï..site)


#############FILTER SITE & CLEAN HER UP
########

#Filter out site that you are interested in HERE#########
        springs_trees_totalPlot <- HG_Trees_totalPlot %>% 
              filter(site == "springsfire")
        springs_trees_diamclass <- HG_Trees_diamclass %>% 
              filter(site == "springsfire")
        springs_trees_plot_species <- HG_Trees_plot_species %>% 
              filter(site == "springsfire")
        springs_trees_severityIndicies <- HG_trees_severityIndicies %>% 
                filter(site == "springsfire")
        springs_trees_all <- HG_Trees_final %>% 
            filter(site == "springsfire")
        
        
##Add burn unit HERE
        str(trt_utm)
        springs_trt <- trt_utm %>% 
                filter(ï..site  == "springsfire") %>% 
                select(plotid, TSLF, burn_unit, burn)
        springs_trees_totalPlot <- left_join(springs_trees_totalPlot, springs_trt,
                                             by="plotid")
        springs_trees_diamclass <- left_join(springs_trees_diamclass, springs_trt,
                                             by="plotid")
        springs_trees_plot_species <- left_join(springs_trees_plot_species, springs_trt,
                                             by="plotid")
        springs_trees_severityIndicies <- left_join(springs_trees_severityIndicies, springs_trt,
                                             by="plotid")
        springs_trees_all <- left_join(springs_trees_all, springs_trt,
                                                    by="plotid")
        
        
#remove all plots in 2013rx burn unit
        
        springs_trees_totalPlot <- springs_trees_totalPlot %>% 
                filter(TSLF != "2013rx")
        springs_trees_diamclass <- springs_trees_diamclass %>% 
                filter(TSLF != "2013rx")
        springs_trees_plot_species <- springs_trees_plot_species %>% 
                 filter(TSLF != "2013rx")
        springs_trees_severityIndicies <- springs_trees_severityIndicies %>% 
          filter(TSLF != "2013rx")
        springs_trees_all <- springs_trees_all %>% 
          filter(TSLF != "2013rx")
       
###################################
        ############# TIME FOR FIGURES!!!
        
        
##################BY WHOLE DIAM CLASS      
        
#match springs to NRV
        NRV_diamclass_YPMC <- NRV_diamclass %>% 
          rename(diamclass = diam_class) %>% 
          distinct() %>%  #justs gives unique values for diamclass & plotTYpe
          mutate(NRV = 'range') %>% 
        #2.47 acre to 1 hectare 
          mutate(NRV_upper_acre = NRV_upper*2.47,
                 NRV_lower_acre = NRV_lower*2.47)
        
       
        
        
        springs_trees_NRV_clean <- springs_trees_diamclass %>% 

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
                group_by(pre_post_fire, diamclass) %>% 
                summarise(n_trees_diam_acre = mean(n_trees_diamclass_acre))
        springs_diamclass_TPA
        
        #STATS
        
        NRVMEAN <- compare_means(n_trees_diamclass_acre ~ pre_post_fire, data = springs_trees_NRV_clean, 
                                 group.by = "diamclass")
        NRVMEAN
        
        
        
        #Create PLOT
       
        
        springs_diamclass_plot <- ggplot(data=springs_trees_NRV_clean)+
          geom_boxplot(aes(x=diamclass, y=n_trees_diamclass_acre, fill=pre_post_fire))+
          geom_errorbar(data= NRV_diamclass_YPMC,
                        aes(x=diamclass, ymin= NRV_lower_acre, ymax=NRV_upper_acre,
                            group = diamclass,
                            color= NRV), #link to group in NRV dataframe
                        width=0.2,
                        size = 1.5)+
          theme_minimal()+
          scale_fill_brewer(palette = "Dark2")+
          theme(axis.title=element_text(size=16,face="bold"),
                axis.text=element_text(size=14),
                axis.title.x = element_blank(),
                plot.title = element_blank())+
          xlab("Diameter Class")+
          ylab("# Trees / Acre")
        springs_diamclass_plot
        
        
        ggsave(plot=springs_diamclass_plot, "HolyGrail/figures/springs/springs_diamclass_NRV_prepost.png")


        
        
        
 #####################       ############################
##COMPARE POST FIRE STRUCTURE BETWEEN BURNED AND UNBURNED PLOTS
        
        ### TOTAL PLOT VALUES
        
        springs_trees_PLOT <- springs_trees_totalPlot %>% 
          filter(status == "LIVE") %>%
          filter(burn =="yes" | burn =="no") %>% 
          mutate(trt_burn = paste(burn, pre_post_fire)) %>% 
          mutate(BA_ft2_acre = BA_m2_acre_plot*10.76,
                 n_trees_acre = n_trees_totalPlot*10) 
        
        springs_trees_PLOT$trt_burn <-  recode(springs_trees_PLOT$trt_burn, 
                                                  "no prefire" = "control",
                                                  "yes postfire" = "post Springs Fire",
                                                  "yes prefire" = "pre Springs Fire") 
        
        springs_trees_PLOT$trt_burn <-  factor(springs_trees_PLOT$trt_burn,
                                                  levels = c("control",
                                                             "pre Springs Fire",
                                                             "post Springs Fire"))
          
        
        #total change
        trees_summary <- springs_trees_PLOT %>%
          group_by(trt_burn) %>% 
          summarise(n_trees_acre = mean(n_trees_acre),
                    BA_m2_acre = mean(BA_m2_acre_plot),
                    BA_ft2_acre = mean(BA_ft2_acre)) 
        trees_summary
        
        #by treatment
        trees_summary_byTRT <- springs_trees_PLOT %>%
          group_by(burn, year, pre_post_fire, TSLF) %>% 
          summarise(n_trees_acre = mean(n_trees_acre),
                    BA_m2_acre = mean(BA_m2_acre_plot),
                    BA_ft2_acre = mean(BA_ft2_acre)) 
        trees_summary_byTRT 
        
        compare_means(n_trees_acre ~ trt_burn, data = springs_trees_PLOT)
        compare_means(BA_ft2_acre ~ trt_burn, data = springs_trees_PLOT)
        
        #Create PLOT
    
        
        springs_trees_control_plot <- ggplot(data=springs_trees_PLOT)+
                geom_boxplot(aes(x=trt_burn, y=n_trees_acre, fill=trt_burn))+
                theme_minimal()+
          scale_fill_brewer(palette = "Dark2")+
                theme(axis.title=element_text(size=16,face="bold"),
                      axis.text=element_text(size=14),
                      axis.title.x = element_blank(),
                      legend.title = element_blank(),
                      legend.text = element_text(size =14))+
                xlab("Burned by Springs Fire")+
                ylab("# Trees / Acre")
         springs_trees_control_plot
       
       ggsave(plot=springs_trees_control_plot, "HolyGrail/figures/springs/springs_trees_control_TPA.png")
       
       
       springs_trees_control_plotBA <- ggplot(data=springs_trees_PLOT)+
               geom_boxplot(aes(x=trt_burn, y=BA_ft2_acre, fill=trt_burn))+
               theme_minimal()+
         scale_fill_brewer(palette = "Dark2")+
               theme(axis.title=element_text(size=16,face="bold"),
                     axis.text=element_text(size=14),
                     axis.title.x = element_blank(),
                     legend.title = element_blank(),
                     legend.text = element_text(size =14))+
               xlab("Burned by Springs Fire")+
               ylab("# Basal Area ft2 / Acre")
       springs_trees_control_plotBA
       
       ggsave(plot=springs_trees_control_plotBA, "HolyGrail/figures/springs/springs_trees_control_BA.png")
       
       
 ## COMPARE SEVERITY INDICIES
       
       tree_scorch_torch <- springs_trees_severityIndicies %>%
               drop_na(TSLF)  %>% 
               group_by(TSLF, species) %>% 
               summarise(scorch_ht_m = mean(avg_scorch_ht_m, na.rm=TRUE),
                         scorch_percent= mean(avg_scorch_percent, na.rm=TRUE),
                         torch_ht_m = mean(avg_torch_ht_m, na.rm=TRUE),
                         torch_percent= mean(avg_torch_percent,, na.rm=TRUE)) 
       tree_scorch_torch
  
####SCORCH TORCH HEIGHT
       springs_trees_scorchtorch_ht <- springs_trees_severityIndicies %>% 
           mutate(plotid_trt =paste(plotid, species, TSLF)) %>% 
           select(plotid_trt, avg_scorch_ht_m, avg_torch_ht_m) %>% 
           pivot_longer(-plotid_trt, names_to= "severity_type", values_to = "ht_m") %>% 
           separate(plotid_trt, c("plotid", "species", "TSLF")) %>% 
           drop_na(ht_m) %>% 
           filter(TSLF != "NA") %>% 
           mutate(TSLF = factor(TSLF, levels= c("nopriorburn",
                                                "2007rx",
                                                "2010rx",
                                                "2013rx"))) %>% 
         mutate(ht_ft = ht_m*3.28)
       
       springs_trees_scorchtorch_ht$severity_type <-  recode(springs_trees_scorchtorch_ht$severity_type, 
                                                  "avg_scorch_ht_m"= "scorch",
                                                  "avg_torch_ht_m"= "torch")
                                                  
       
       springs_trees_scorchtorchht_plot <- ggplot(data=springs_trees_scorchtorch_ht)+
           geom_boxplot(aes(x=severity_type, y=ht_ft, fill=TSLF))+
           theme_minimal()+
         scale_fill_brewer(palette = "Dark2")+
           theme(axis.title=element_text(size=16,face="bold"),
                 axis.text=element_text(size=16),
                 axis.title.x = element_blank(),
                 legend.title = element_blank(),
                 legend.text = element_text(size =14))+
           ylab("Height (ft)")
       springs_trees_scorchtorchht_plot
       
       ggsave(plot= springs_trees_scorchtorchht_plot, "HolyGrail/figures/springs/springs_trees_scorchtorch.png")
       
       compare_means(ht_ft ~ TSLF, data = springs_trees_scorchtorch_ht, 
                     group.by = "severity_type")
       
####SCORCH TORCH PERCENT
       springs_trees_scorchtorch_percent <- springs_trees_severityIndicies %>% 
           mutate(plotid_trt =paste(plotid, species, TSLF)) %>% 
           select(plotid_trt, avg_scorch_percent, avg_torch_percent) %>% 
           pivot_longer(-plotid_trt, names_to= "severity_type", values_to = "percent") %>% 
           separate(plotid_trt, c("plotid", "species", "TSLF")) %>% 
           drop_na(percent) %>% 
           filter(TSLF != "NA") %>% 
           mutate(TSLF = factor(TSLF, levels= c("nopriorburn",
                                                "2007rx",
                                                "2010rx",
                                                "2013rx"))) 
       
       springs_trees_scorchtorch_percent$severity_type <-  recode(springs_trees_scorchtorch_percent$severity_type, 
                                                             "avg_scorch_percent"= "scorch",
                                                             "avg_torch_percent"= "torch")
       
       
       springs_trees_scorchtorchpercent_plot <- ggplot(data=springs_trees_scorchtorch_percent)+
           geom_boxplot(aes(x=severity_type, y=percent, fill=TSLF))+
           theme_minimal()+
         scale_fill_brewer(palette = "Dark2")+
           theme(axis.title=element_text(size=16,face="bold"),
                 axis.text=element_text(size=16),
                 axis.title.x = element_blank(),
                 legend.title = element_blank(),
                 legend.text = element_text(size =14))+
           ylab("Percent (%)")
       springs_trees_scorchtorchpercent_plot
       
       ggsave(plot= springs_trees_scorchtorchpercent_plot, "HolyGrail/figures/springs/springs_trees_scorchtorch_percent.png")
       
       compare_means(percent ~ TSLF, data = springs_trees_scorchtorch_percent, 
                     group.by = "severity_type")
       
####BOLE CHAR
       springs_trees_bolechar <- springs_trees_severityIndicies %>% 
           select(plotid, TSLF, avg_bolechar_m) %>% 
           drop_na(avg_bolechar_m) %>% 
           filter(TSLF != "NA") %>% 
           mutate(TSLF = factor(TSLF, levels= c("nopriorburn",
                                                "2007rx",
                                                "2010rx",
                                                "2013rx"))) %>% 
           rename(BoleChar = avg_bolechar_m ) %>% 
            mutate(bolechar_ft = BoleChar*3.28)
       
   
       
       
       springs_trees_bolechar_plot <- ggplot(data=springs_trees_bolechar)+
           geom_boxplot(aes(x=TSLF, y=bolechar_ft, fill=TSLF))+
           theme_minimal()+
         scale_fill_brewer(palette = "Dark2")+
           theme(axis.title=element_text(size=16,face="bold"),
                 axis.text=element_text(size=16),
                 axis.title.x = element_blank(),
                 legend.position = 'none')+
           ylab("Bole Char Height (ft)")
       springs_trees_bolechar_plot
       
       ggsave(plot= springs_trees_bolechar_plot, "HolyGrail/figures/springs/springs_trees_bolechar.png")
       
       compare_means(bolechar_ft ~ TSLF, data = springs_trees_bolechar)
       
##### HEIGHT TO CROWN 
       
       
       springs_trees_ht2crown <- springs_trees_all %>% 
           select(plotid, pre_post_fire, postFire, TSLF, burn,
                  status, species, ht2crown_m)
       
       springs_trees_ht2crown_burn <- springs_trees_ht2crown %>% 
           filter(burn == "yes") %>% 
           mutate(TSLF = factor(TSLF, levels= c("nopriorburn",
                                                "2007rx",
                                                "2010rx",
                                                "2013rx"))) %>% 
         mutate(pre_post_fire = factor(pre_post_fire, levels = c("prefire",
                                                                 "postfire"))) %>% 
         mutate(ht2crown_ft = ht2crown_m*3.28)
         
         
           
       
       springs_trees_ht2crownBurn_plot <- ggplot(data=springs_trees_ht2crown_burn)+
           geom_boxplot(aes(x=TSLF, y=ht2crown_ft, fill=pre_post_fire))+
           theme_minimal()+
           scale_fill_brewer(palette = "Dark2")+
           theme(axis.title=element_text(size=16,face="bold"),
                 axis.text=element_text(size=16),
                 axis.title.x = element_blank(),
                 legend.title = element_blank(),
                 legend.text = element_text(size=14))+
           ylab("Height to Live Crown (ft)")
       springs_trees_ht2crownBurn_plot
       
       ggsave(plot= springs_trees_ht2crownBurn_plot, "HolyGrail/figures/springs/springs_trees_ht2crownBURN.png")
       
       #STATS
       
       compare_means(ht2crown_ft ~ pre_post_fire, data = springs_trees_ht2crown_burn, 
                     group.by = "TSLF")
      
       

       