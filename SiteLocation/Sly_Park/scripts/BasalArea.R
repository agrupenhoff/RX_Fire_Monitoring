install.packages(c("tidyr","devtools"))

Pre_Springs_BA<-read.csv("R/SpringsFire/Pre_Springs_BA.csv", header = TRUE, stringsAsFactors=F)
Pre_Springs_BA

###organize ja data
library(tidyr)

    #combine species and status
    Pre_Springs_BA$SpeciesStatus <- paste(Pre_Springs_BA$Species,
                                          "_", Pre_Springs_BA$Status)

    Pre_Springs_BA$SpeciesStatus
    
    #organize by species and status


###Basal Area (Ft2/acre)
    
    
    prespringsba<-NULL
    prespringsba <-as.data.frame(matrix(NA, ncol=5, nrow=nrow(Pre_Springs_BA)))
    colnames(prespringsba)<-c("plot_id","pije_live", "pije_dead", "pico_live", "pico_dead")
    
    #run a forloop to apply the equations to each row in the data frame
    for (i in 1:nrow(Pre_Springs_BA)) {
      
      prespringsba[i,"plot_id"]<-Pre_Springs_BA[i,"plot_id"]          
      prespringsba[i,"pije_live"]<- if (Pre_Springs_BA$SpeciesStatus=pije_live){
            print("basalarea")} else{
              print('0')}
                                       
      prespringsba[i,"pije_dead"]<- Pre_Springs_BA[i,"basalarea"]
          if (Pre_Springs_BA$SpeciesStatus=pije_dead)
             {print(Pre_Springs_BA[i,"basalarea"])} else{print('0')}
                                       
