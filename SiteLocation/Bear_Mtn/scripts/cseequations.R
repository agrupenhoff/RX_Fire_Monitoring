

getwd()
Pre_SpringsFire_FUELS <- read.csv("data/Pre_SpringsFire_FUELS.csv", header = TRUE, stringsAsFactors=F)
Pre_SpringsFire_FUELS


library(dplyr)


##Browns equations
          # Sq diam        specific grav   angle corr fact   length of line (ft)
          # 1 hr: 0.0151     0.48            1.13             2*3.28
          # 10 hr: 0.289    0.48            1.13              2*3.28
          # 100 hr: 2.76   0.40            1.13               4*3.28
          # cwd sound       0.40            1.00              11.3*3.28
          # cwd rotten      0.30            1.00              11.3*3.28
          
          # slope correction = (1+(presprings_fuelslope/100)^2)^(1/2)
          slopecorrection= (1+(Pre_SpringsFire_FUELS$slope_percent/100)^2)^(1/2) #QAQC on error in slope
          slopecorrection
          # convert sq cm to sq in: * 0.155

#VKD applying the Browns equation          
#make a new object to hold all this together    
#new object "ashley" has the mass at each timeframe/category
prespringsfuels<-NULL
prespringsfuels<-as.data.frame(matrix(NA, ncol=6, nrow=nrow(Pre_SpringsFire_FUELS)))
colnames(prespringsfuels)<-c("plot_id","mass_1hr", "mass_10hr", "mass_100hr", "mass_cwd_sound", "mass_cwd_rotten")

#run a forloop to apply the equations to each row in the data frame
for (i in 1:nrow(Pre_SpringsFire_FUELS)) {
	      
  prespringsfuels[i,"plot_id"]<-Pre_SpringsFire_FUELS[i,"plot_id"]          
  prespringsfuels[i,"mass_1hr"]<- ((11.64 * Pre_SpringsFire_FUELS[i,"count_x1h"] * 0.0151 * 	0.48 * 1.13 * slopecorrection[i])/ (2*3.28))         
  prespringsfuels[i,"mass_10hr"]<-((11.64 * Pre_SpringsFire_FUELS[i,"count_x10h"] * 0.289 * 	0.48 * 1.13 * 1) / (2*3.28))        
  prespringsfuels[i,"mass_100hr"]<-(11.64 * Pre_SpringsFire_FUELS[i,"count_x100h"] * 2.76 * 	0.40 * 1.13 * 1) / (4*3.28)           
  prespringsfuels[i,"mass_cwd_sound"]<- ((11.64 * Pre_SpringsFire_FUELS[i,"sum_d2_1000s_cm2"] 	* 0.155 * 0.40 * 1) / (11.3*3.28))           
  prespringsfuels[i,"mass_cwd_rotten"]<-(11.64 * Pre_SpringsFire_FUELS[i,"sum_d2_1000r_cm2"] 	* 0.155 * 0.30 * 1) / (11.3*3.28)    
        
}      
   prespringsfuels
                   
##Ideally would like to call function to average mass for each plot, then compare to post plot 

#VKD: now get averages of each mass type per plot by just using aggregate

plotavgs<-aggregate(prespringsfuels[,2:6], list(prespringsfuels$plot_id), mean)  

plotavgs


#VKD: ^ this is now a dataframe of the average per plot mass at each timeframe/category - if you have your data structured like this, it's easy to use aggregate to apply a simple function like mean by categories in one of your columns

#Combine Treatment arrays

        "control" <- rbind(plotavgs[1:2,], plotavgs[10:18,], plotavgs[23,],plotavgs[26,], plotavgs[29:35,])
        '2007rx' <-(plotavgs[3:9,])
        "2010rx" <- rbind(plotavgs[19:22,], plotavgs[24,])
        "2013rx" <-rbind(plotavgs[25,], plotavgs[27:28,])
        
        `2007rx`
        `2010rx`
        control
        `2013rx`
        

        
        "2007plotavgs" <- colMeans(`2007rx`[sapply(`2007rx`, is.numeric)])
        "2010plotavgs" <- colMeans(`2010rx`[sapply(`2007rx`, is.numeric)])
        "2013plotavgs" <- colMeans(`2013rx`[sapply(`2007rx`, is.numeric)])
        "controlplotavgs" <- colMeans(control[sapply(`2007rx`, is.numeric)])
        
        `2007plotavgs`
        `2010plotavgs`
        `2013plotavgs`
        controlplotavgs
      

####Graphing
       
                