library(dplyr)
library(plyr)
library(vegan)


HG_species <- import("HolyGrail/data/clean/HG_species_clean.csv")
springs_trt <- read.csv("SiteLocation/Springs_Fire/data/clean/springs_trt.csv")


                      ##UNDERSTORY and SHRUB SPECIES ONLY NO TREES 



                       springs_species <- HG_species %>% 
                        filter(site == "springsfire") %>% 
                        filter(status == "live") %>% 
                        filter(lifeform =="SH"|
                                 lifeform =="FB"|
                                 lifeform=="GR") %>% 
                        mutate(plot_id = tolower(plot_id)) %>% 
                        mutate(plotid_time=paste(plot_id, pre_post_fire)) %>% 
                        select(plotid_time, species, percent) %>% 
                        pivot_wider(id_cols="plotid_time", names_from="species",
                                    values_from="percent",
                                    values_fill = 0,
                                    values_fn=sum)
                      
                      glimpse(springs_species)
                      
                   export(springs_species,"SiteLocation/Springs_Fire/data/clean/springs_species.csv" )



######RICHNESS
#sum up the number of non-zero entries per row 
#first column is ignored ([,-1]) since this is the site name, not species count
springs_richness <- ddply(springs_species,~plotid_time,function(x) {
     data.frame(RICHNESS=sum(x[-1]>0))})

springs_richness <- springs_richness %>% 
  separate(plotid_time, c("plotid","pre_post_fire"))

springs_richness <- left_join(springs_richness, springs_trt, by="plotid")

#MAKE LOGICAL SENSE
springs_richness <- springs_richness %>% 
  filter(TSLF != "2013rx") %>% 
  mutate(pre_post_fire = factor(pre_post_fire, levels=c('prefire',
                                                        'postfire'))) %>% 
  mutate(TSLF = factor(TSLF, levels= c("nopriorburn",
                                       "2007rx",
                                       "2010rx",
                                       "2013rx"))) %>% 
  mutate(PriorBurn = case_when(
    TSLF == '2007rx' ~ "priorburn",
    TSLF == '2010rx' ~ "priorburn",
    TSLF == '2013rx' ~ "priorburn",
    TSLF == 'nopriorburn' ~ "nopriorburn"))

###############################
####SPECIES RICHNESS IN BURNED PLOTS ONLY

springs_rich_burn <- springs_richness %>% 
  filter(burn=="yes")

springs_species_burned <- ggplot(data=springs_rich_burn, 
                                   aes(x=TSLF, y=RICHNESS, fill=pre_post_fire))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=16,face="bold"),
        axis.text=element_text(size=14,angle = 45, hjust=1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size =14))
springs_species_burned

ggsave(plot=springs_species_burned, "HolyGrail/figures/springs/springs_richness_burned.png")


compare_means(RICHNESS ~ pre_post_fire, data = springs_rich_burn, 
              group.by = "TSLF")

compare_means(RICHNESS ~ TSLF, data = springs_rich_burn, 
              group.by = "pre_post_fire")


springs_species_burned2 <- ggplot(data=springs_rich_burn, 
                                 aes(x=PriorBurn, y=RICHNESS, fill=pre_post_fire))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,angle = 45, hjust=1))
springs_species_burned2

        
          rich_burn <- lmer(RICHNESS ~ PriorBurn*pre_post_fire + 
                                  (1|burn_unit) , data = springs_rich_burn)
          summary(rich_burn)
          anova(rich_burn)
                    ##Potentially block by burn unit? But burn unit is basically correlated TSLF, 
                    ##might already be taken into account 
          
          #multiple comparisons using multiple pairwise-comparisons between means of groups
          rich_burn.emm <- emmeans(rich_burn, 
                                   specs = pairwise ~ PriorBurn:pre_post_fire)
          plot(rich_burn.emm)
         


###############################
####SPECIES RICHNESS BURN VS CONTROL

springs_rich_control <- springs_richness %>% 
  filter(pre_post_fire=="postfire") 

springs_species_controlplot <- ggplot(data=springs_rich_control, 
                                 aes(x=burn, y=RICHNESS))+
  geom_boxplot()+
  geom_jitter()+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,angle = 45, hjust=1))
springs_species_controlplot


################################
#########SPECIES DIVERSITY

springs_div_shan <- ddply(springs_species,~plotid_time,function(x) {
  data.frame(SHANNON=diversity(x[-1], index="shannon"))})

springs_div_shan <- springs_div_shan %>% 
  separate(plotid_time, c("plotid","pre_post_fire")) %>% 
  mutate(pre_post_fire = factor(pre_post_fire, levels=c('prefire',
                                                        'postfire')))

springs_div_shan <- left_join(springs_div_shan, springs_trt, by="plotid")


springs_div_shan <- springs_div_shan %>% 
  filter(TSLF != "2013rx") %>% 
  mutate(TSLF = factor(TSLF, levels= c("nopriorburn",
                                       "2007rx",
                                       "2010rx",
                                       "2013rx"))) %>% 
  mutate(PriorBurn = case_when(
    TSLF == '2007rx' ~ "priorburn",
    TSLF == '2010rx' ~ "priorburn",
    TSLF == '2013rx' ~ "priorburn",
    TSLF == 'nopriorburn' ~ "nopriorburn"))

##########################
#COMPARE DIVERSITY BY BETWEEN BURN SITES

springs_divShan_burn <- springs_div_shan %>% 
  filter(burn=="yes")

springs_divShan_burnplot <- ggplot(data=springs_divShan_burn, 
                                 aes(x=TSLF, y=SHANNON, fill=pre_post_fire))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=17,face="bold"),
        axis.text=element_text(size=14,angle = 45, hjust=1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size =14))+
  ylab("Species Diversity (shannon)")
springs_divShan_burnplot 

ggsave(plot=springs_divShan_burnplot , "HolyGrail/figures/springs/springs_shanDiversity_burned.png")

compare_means(SHANNON ~ pre_post_fire, data = springs_divShan_burn, 
              group.by = "TSLF")

compare_means(SHANNON ~ TSLF, data = springs_divShan_burn, 
              group.by = "pre_post_fire")

springs_divShan_burnplot2 <- ggplot(data=springs_divShan_burn, 
                                   aes(x=PriorBurn, y=SHANNON, fill=pre_post_fire))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=18,face="bold"),
        axis.text=element_text(size=16, hjust=1),
        axis.title.x = element_blank(),
        plot.title = element_blank(),        #remove plot title
        strip.text.x = element_blank(),      #remove facet wrap title
        legend.title = element_blank())+     #remove legend title
  ylab("Species Diversity")
springs_divShan_burnplot2 

ggsave(plot=springs_divShan_burnplot2, "HolyGrail/figures/springs/springs_divShan_burned_trtBASIC.tiff", dpi=300)


                #BASICALLY THE BURN HELPED INCREASE SHAN DIVERSITY FOR 
                #PLOTS THAT DIDNT BURN

                SHAN_burn <- lmer(SHANNON ~ PriorBurn*pre_post_fire + 
                                    (1|burn_unit) , data = springs_divShan_burn)
                summary(SHAN_burn)
                anova(SHAN_burn)
                ##Potentially block by burn unit? But burn unit is basically correlated TSLF, 
                ##might already be taken into account 
                
                #multiple comparisons using multiple pairwise-comparisons between means of groups
                SHAN_burn.emm <- emmeans(SHAN_burn, 
                                         specs = pairwise ~ PriorBurn:pre_post_fire)
                plot(SHAN_burn.emm)






#######DIVERSITY BURN VS CONTROL


springs_divShan_control <- springs_div_shan %>% 
  filter(pre_post_fire=="postfire")

springs_divShan_controlplot <- ggplot(data=springs_divShan_control, 
                                   aes(x=burn, y=SHANNON))+
  geom_boxplot()+
  geom_jitter()+
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme_minimal()+
  stat_compare_means(aes(label = ..p.signif..))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,angle = 45, hjust=1))
springs_divShan_controlplot 






##################################################################
#####SPECIES EVENNESS

springs_div_simp <- ddply(springs_species,~plotid_time,function(x) {
  data.frame(SIMPSON=diversity(x[-1 ], index="simpson")/log(sum(x[-1 ]>0)))})

springs_div_simp <- springs_div_simp %>% 
  separate(plotid_time, c("plotid","pre_post_fire"))

springs_div_simp <- left_join(springs_div_simp, springs_trt, by="plotid")

##########################
#COMPARE DIVERSITY BY BETWEEN BURN SITES

springs_divSimp_burn <- springs_div_simp %>% 
  filter(burn=="yes")

springs_divSimp_burnplot <- ggplot(data=springs_divSimp_burn, 
                                   aes(x=burn_unit, y=SIMPSON, fill=pre_post_fire))+
  geom_boxplot()+
  geom_jitter()+
  scale_fill_manual(values=wes_palette("BottleRocket1"))+
  theme_minimal()+
  stat_compare_means(aes(label = ..p.signif..))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,angle = 45, hjust=1))
springs_divSimp_burnplot 

              ###SAME RESULT AS SHANNONS
              ### WHAT ABOUT SCALE?


