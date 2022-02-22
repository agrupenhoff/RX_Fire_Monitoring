library(dplyr)
library(plyr)
library(vegan)
library(ggplot2)
library(tidyverse)
library(tibble)
library(ggpubr)


HG_species <- read.csv("HolyGrail/data/clean/HG_species_clean.csv")
VR_plottype <- read.csv("SiteLocation/Valentine/data/clean/VR_plottype.csv")


##UNDERSTORY and SHRUB SPECIES ONLY NO TREES 

val_species <- HG_species %>% 
  mutate(lifeform = toupper(lifeform)) %>% 
  mutate(species = tolower(species)) %>% 
  filter(site == "Valentine") %>% 
  filter(lifeform =="SH"|
           lifeform =="FB"|
           lifeform=="GR")  %>% 
  mutate(plotid_time=paste(plot_id, pre_post_thin)) %>% 
  select(plotid_time, species, percent) %>% 
  pivot_wider(id_cols="plotid_time", names_from="species",
              values_from="percent",
              values_fill = 0,
              values_fn=sum)


export(val_species,"SiteLocation/Valentine/data/clean/val_species2021.csv" )



######RICHNESS
#sum up the number of non-zero entries per row 
#first column is ignored ([,-1]) since this is the site name, not species count
val_richness <- ddply(val_species,~plotid_time,function(x) {
  data.frame(RICHNESS=sum(x[-1]>0))})

val_richness <- val_richness %>% 
  separate(plotid_time, c("plotid","pre_post_thin"))

val_richness <- left_join(val_richness, VR_plottype, by="plotid")

#MAKE LOGICAL SENSE
val_richness <- val_richness %>% 
  mutate(pre_post_thin = factor(pre_post_thin, levels=c('prethin',
                                                        'postthin'))) 

###############################
####SPECIES RICHNESS IN BURNED PLOTS ONLY

val_species_thin <- ggplot(data=val_richness, 
                                 aes(x=pre_post_thin, y=RICHNESS, fill=pre_post_thin))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=16,face="bold"),
        axis.text=element_text(size=14,angle = 45, hjust=1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size =14))
val_species_thin

ggsave(plot=val_species_thin, "HolyGrail/figures/valentine/val_richness_thinned.png")

thinSum <- val_richness %>% 
  drop_na(RICHNESS) %>% 
  group_by(pre_post_thin, PlotType) %>% 
  dplyr::summarise(mean=mean(RICHNESS),
                   std = sd(RICHNESS))

compare_means(RICHNESS ~ pre_post_thin, data = val_richness)

val_richness_vegtype <- val_richness %>% 
  drop_na("PlotType")

val_rich_plottype <- ggplot(data=val_richness_vegtype, 
                               aes(x=pre_post_thin, y=RICHNESS, fill=PlotType))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=17,face="bold"),
        axis.text=element_text(size=14,angle = 45, hjust=1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size =14))
val_rich_plottype

ggsave(plot=val_rich_plottype, "HolyGrail/figures/valentine/val_richness_plottype_thinned.png")

compare_means(RICHNESS ~ pre_post_thin, data = val_richness,
              group.by = "PlotType")
compare_means(RICHNESS ~ pre_post_thin, data = val_richness)







################################
#########SPECIES DIVERSITY

val_div_shan <- ddply(val_species,~plotid_time,function(x) {
  data.frame(SHANNON=diversity(x[-1], index="shannon"))})

val_div_shan <- val_div_shan %>% 
  separate(plotid_time, c("plotid","pre_post_thin")) %>% 
  mutate(pre_post_thin = factor(pre_post_thin, levels=c('prethin',
                                                        'postthin')))

val_div_shan <- left_join(val_div_shan, VR_plottype, by="plotid")


##########################
#COMPARE DIVERSITY BY BETWEEN BURN SITES



val_divShan_plot <- ggplot(data=val_div_shan, 
                                   aes(x=pre_post_thin, y=SHANNON, fill=pre_post_thin))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=17,face="bold"),
        axis.text=element_text(size=14,angle = 45, hjust=1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size =14))+
  ylab("Species Diversity (shannon)")
val_divShan_plot 

ggsave(plot=val_divShan_plot , "HolyGrail/figures/valentine/val_shanDiversity_thinned.png")

val_divshan_vegtype <- val_div_shan %>% 
  drop_na("PlotType")

val_divShan_plottype <- ggplot(data=val_divshan_vegtype, 
                               aes(x=pre_post_thin, y=SHANNON, fill=PlotType))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=17,face="bold"),
        axis.text=element_text(size=14,angle = 45, hjust=1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size =14))+
  ylab("Species Diversity (shannon)")
val_divShan_plottype

ggsave(plot=val_divShan_plottype , "HolyGrail/figures/valentine/val_shanDiversity_plottype_thinned.png")

compare_means(SHANNON ~ pre_post_thin, data = val_div_shan)

compare_means(SHANNON ~ TSLF, data = springs_divShan_burn, 
              group.by = "pre_post_fire")

springs_divShan_burnplot2 <- ggplot(data=springs_divShan_burn, 
                                    aes(x=PriorBurn, y=SHANNON, fill=pre_post_fire))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,angle = 45, hjust=1),
        axis.title.x = element_blank(),
        plot.title = element_blank(),        #remove plot title
        strip.text.x = element_blank(),      #remove facet wrap title
        legend.title = element_blank())+     #remove legend title
  ylab("Species Diversity")
springs_divShan_burnplot2 

ggsave(plot=springs_divShan_burnplot2, "HolyGrail/figures/springs/springs_divShan_burned_trtBASIC.png")


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


