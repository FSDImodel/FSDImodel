#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# !! READ.me !!
#
# FSDI model main V1.1
# Authors: Joep Bosdijk (joepbosdijk@outlook.com), Annika Gaiser (annika.gaiser@web.de)
# Group members: Joep Bosdijk, Annika Gaiser, Vincent de Feiter, Gudrun Torkelsdottir, Thijs Smink
#
# This model can create the following outputs:
# - calculate the bloom date for stations in NL
# - calculate when the chilling and forcing threshold is reached, which marks the start of the growing season and blooming for stations in NL
# - calculate the frost damage per year, divided in the categories mild, moderate, and severe
# - calculate the RMSE of the model performance and plot the modelled against the observed values of the bloom date
#
# The following input files are required:
# - hourly temperature of meteorological stations; downloaded from KNMI and adapted via the script "TransformKNMItoFile.R" (filename automatically done) (structure: "Year"[int],"Month"[int],"Day"[int],"Hour"[int],"Temp"[num])
# - species/variety specific model parameters, created with the script "Make_model_parameters.R" (filename automatically done)
# - observational phenology data (bloom dates) (filename: Applevariety_possiblefilenameadditions.txt) (structure: Year[int],JDay[int],Date[chr, "dd-mm"],DD-MM-YYYY[chr, "dd-m(m)-yyyy"])
#
# How to use the model:
# - adapt below the desired outputs and input values, run then the complete script (e.g. with Ctrl+Shift+S)
#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# load required libraries; if error is given, the libraries need to be installed
library(readr)
library(tidyr)
library(chillR)
library(ggplot2)
library(patchwork)
require(reshape2)
library(data.table)
library(readxl)
library(lubridate)
library(ggpubr)
require(grid)
theme_set(theme_pubr())

rm(list=ls()) # remove variable names in environment; recommended before starting a new run



###########################################################################################
##### CHANGE BELOW: paths, folders, apple variety, desired output, station name, year #####
###########################################################################################

## define folder paths (if folder directory same as main path, just use empty "")
path <- "C:\\FSDI_model\\" # path to the main folder
HourlyTemp_folder <- "hourly_data\\" # temperature data
pheno_folder <- "phenological data\\" # apple observation files 
model_param_folder <- "Parameters\\" # model parameters
ClimateScenario_folder <- "Climate_change_scenario_data\\" # temperature climate data 
output <- "Output\\" # define folder where created images/files are to be saved

#----------------------------------------------------------------------------------
## specify apple variety -> will be used to load file and will be added to created plots & files
applevar <- "Boskoop" # Boskoop, Cox, Golden_Delicious, James_Grieve
# if observational data filename includes specifications after the apple variety name
applevar_filenameaddition <- "_filtered_stations_2010" #change: _filtered_stations_2010 (Boskoop), _filtered_stations_2004 (Cox), _filtered_stations_2015 (Golden Delicious), _filtered_stations_2000 (James Grieve); _coast (for coastal phenology data)

#----------------------------------------------------------------------------------
## Define in the following which sections you want to run  ->  "code <- c(1,no.)"
# 1: REQUIRED (loading data, model parameters & running frost damage function)
# 2: plot of bloom date time series (model & obs.) for one station
# 3: plot of bloom date time series (model) for multiple stations
# 4: plot of chill and forcing for one station for one year
# 5: plot of chill and forcing for multiple stations for one year
# 6: file of budburst, bloom, and frost damage
# 7: RMSE of model for one station & plot model against observations
code <- c(1,4)

#----------------------------------------------------------------------------------
## Depending on the chosen sections, adapt the following:
# 2,4: Bloom date one station
name <- c("DeBilt") # station name

# 3: Bloom date multiple stations - 4 stations must be chosen
station <- c("DeBilt","DeKooy","Eelde","Maastricht")

# 4,5: Chill & Forcing
end_season_year<-1991

# 6: File creation - define stations for which model output files are to be created
    # should be the same as name2 in "TransformKNMItoFile.R" 
    # if an error occurs in this section it is likely due to the temperature file only consisting of NAs. This station name should then be removed
station_file <- c("Arcen","Berkhout","Cabauw","DeBilt","DeKooy","Deelen","Eelde","Eindhoven","Ell","GilzeRijen","Heino","Herwijnen","HoekvanHolland","Hoogeveen","Hupsel","IJmuiden","Lauwersoog","Leeuwarden","Maastricht","Marknesse","NieuwBeerta","RdamGeulhaven","Rotterdam","Schiphol","Soesterberg","Stavoren","Terschelling","Twenthe","ValkenburgZH","Vlieland","Vlissingen","Volkel","Voorschoten","Westdorpe","WijkaanZee","Wilhelminadorp","Woensdrecht")

# 7: RMSE & model against obs. plot
name_RMSE <- c("DeBilt") # DeBilt (more inland, warmer, model fitted for this station) or Rotterdam (near the coast, colder)
n2 <- 2 #default = 2; 1 to run over all years, 2 for only half of the years (to check non-paramterisation, years the model was not fitted against)


############################################################################################
##### END CHANGES ##########################################################################
############################################################################################


# If more observational phenology data is available lines 127-146 need to be adapted


#=====================================================================================================================
#=====================================================================================================================
#=====================================================================================================================




############################################################################################
##### START MODEL ##########################################################################
############################################################################################

for (c in 1:length(code)){

#######################################
## Load data & frost damage function ##-------------------------------------------------------------------------------------------
#######################################

  if (1 == code[c]){
    print("Load data & function")
    #load the phenological data & define JDay & years available in data
    Observations <- read.table(paste(path,pheno_folder,applevar,applevar_filenameaddition,".txt", sep = ""), sep=",", header=1)
    par <- read_tab(paste(path,model_param_folder,"PhenoFlex_parameters_",applevar,".csv", sep=""))[,2]
    
    JDay <- c()
    JDay <- Observations$JDay
    
    
    #specify years available in data
    if (applevar_filenameaddition == "_coast"){  
      if (applevar == "Boskoop"){
        years = append(1983,c(1985:1990))
      }else if (applevar == "Cox"){
        years = append(c(1974:1983),c(1985:1990))
      }else if (applevar == "Golden_Delicious"){
        years <-  append(c(1974:1983),c(1985:1990))
      }else if (applevar == "James_Grieve"){
        years <- append(c(1974:1983),c(1986:1990))
      }
      Observations <- Observations[Observations$Year %in% years,]
    }else{
      if (applevar == "Boskoop"){
        years = c(1958:2010)
      }else if (applevar == "Cox"){
        years = c(1958:2004)
      }else if (applevar == "Golden_Delicious"){
        years <-  append(c(1958:2012),c(2014:2015))
      }else if (applevar == "James_Grieve"){
        years <- c(1958,2000)
      }    
    }
    
    yc = par[1]
    zc = par[2]

  

########################################################
## Input for calculating frost index with Cherry data ##---------------------------------
########################################################


    Silver_tip <- 0.19*zc
    Green_tip <- 0.45*zc
    half_green <- 0.525*zc
    Tight_cluster <- 0.6*zc
    Pink <- 0.73*zc
    
    
    Temp_Silver_tip_old <- -8.89
    Temp_Silver_tip_10 <- -9.44
    Temp_Silver_tip_90 <- -16.67
    
    Temp_Green_tip_old <- -8.88
    Temp_Green_tip_10 <-  -8.89
    Temp_Green_tip_90 <- -12.22
    
    Temp_half_green_old <- -5
    Temp_half_green_10 <-  -5.01
    Temp_half_green_90 <- -9.44
    
    Temp_Tight_cluster_old <- -2.77
    Temp_Tight_cluster_10 <- -2.78
    Temp_Tight_cluster_90 <- -6.11
    
    Temp_Pink_old <- -2.2
    Temp_Pink_10 <- -2.22
    Temp_Pink_90 <- -3.89
    
    Temp_bloom_old <- -2.2
    Temp_bloom_10 <- -2.22
    Temp_bloom_90 <- -3.89
    
    
    frost_index <- function(z,Temp, JDay, Bloom,T){
      Damage_index = 0
      Stage <<-  "Dormant"
      if (Silver_tip <= z & z <Green_tip){
        Stage <<-  "Silver"
        if (Temp_Silver_tip_old>= Temp & Temp> Temp_Silver_tip_10){Damage_index <- 1}
        else if (Temp_Silver_tip_10>= Temp & Temp> Temp_Silver_tip_90){Damage_index <- 10}
        else if (Temp_Silver_tip_90>= Temp){Damage_index <- 100}
        else {Damage_index <- 0}
      }else if (Green_tip<= z & z<half_green){
        Stage <<-  "Green tip"
        if (Temp_Green_tip_old >= Temp & Temp> Temp_Green_tip_10){Damage_index <- 1}
        else if (Temp_Green_tip_10>= Temp & Temp> Temp_Green_tip_90){Damage_index <- 10}
        else if (Temp_Green_tip_90>= Temp){Damage_index[1] <- 100}
        else {Damage_index <- 0}
      }else if (half_green<= z & z<Tight_cluster){
        Stage <<-  "Half_green"
        if (Temp_half_green_old >= Temp & Temp > Temp_half_green_10){Damage_index <- 1}
        else if (Temp_half_green_10>= Temp & Temp> Temp_half_green_90){Damage_index <- 10}
        else if (Temp_half_green_90>= Temp){Damage_index[1] <- 100}
        else {Damage_index <- 0}
      }else if (Tight_cluster<= z & z <Pink){
        Stage <<-  "Tight cluster"
        if (Temp_Tight_cluster_old>= Temp & Temp> Temp_Tight_cluster_10){Damage_index <- 1}
        else if (Temp_Tight_cluster_10>= Temp & Temp> Temp_Tight_cluster_90){Damage_index <- 10}
        else if (Temp_Tight_cluster_90>= Temp){Damage_index <- 100}
        else {Damage_index <- 0}
      }else if (Pink<= z & z < zc){
        Stage <<-  "Pink"
        if (Temp_Pink_old>= Temp & Temp> Temp_Pink_10){Damage_index <- 1}
        else if (Temp_Pink_10>= Temp & Temp> Temp_Pink_90){Damage_index <- 10}
        else if (Temp_Pink_90>= Temp){Damage_index <- 100}
        else {Damage_index <- 0}
      } else if (JDay >= Bloom & JDay<200){
        Stage <<-  "Bloom"
        if (Temp_bloom_old >= Temp & Temp > Temp_bloom_10){Damage_index <- 1}
        else if (Temp_bloom_10 >= Temp & Temp > Temp_bloom_90){Damage_index <- 10}
        else if (Temp_bloom_90 >= Temp){Damage_index <- 100}
        else {Damage_index <- 0}
      }
      season_Jday$Stage[T] <<- Stage
      return(Damage_index)
    }
    
    
  }
  
###############################################
## Bloom date single station plot incl. obs. ##-----------------------------------------------------------------------------------
###############################################
  
  if (2 == code[c]){
  print(paste("Bloom date plot for ",name))  
    for (i in name){
      hourtemps <- read.table(paste(path,HourlyTemp_folder,"hourly_temperature_",i,".txt", sep = ""), sep=",", header=1)
      hourtemps <- hourtemps[!is.na(hourtemps$Temp),] # remove NA temperature values
      
      #run model for the specified growing seasons
      seasons<-years
      
      iSeason <- genSeason(hourtemps,
                           mrange = c(8, 6),
                           years=seasons)
      
      #lists needed for damage indicator & chilling hours
      CR_full <- vector(mode='list', length=length(seasons))
      Bloom <- vector(mode='list', length=(length(seasons)))
      
      #run the model over all years previously specified
      for (sea in 1:(length(seasons))){
        try({
          season_data<-hourtemps[iSeason[[sea]],]
          
          res <- PhenoFlex(temp=season_data$Temp,
                           times=c(1: length(season_data$Temp)),
                           stopatzc=TRUE,
                           basic_output=FALSE,
                           deg_celsius = TRUE,
                           yc = par[1], zc = par[2], s1 = par[3],
                           Tu = par[4], E0 = par[5], E1 = par[6],
                           A0 = par[7], A1 = par[8], Tf = par[9],
                           Tc = par[10], Tb =par[11], slope = par[12])
          
          #Use Date + these paramteres + Temperature to make frost damage index
          season_data[,"x"]<-res$x
          season_data[,"y"]<-res$y
          season_data[,"z"]<-res$z
          season_data <- add_date(season_data)  
          season_Jday <- make_JDay(season_data)
          
          CR_full[[sea]]<-season_Jday$JDay[which(season_Jday$y>=yc)[1]]
          Bloom[[sea]]<-season_Jday$JDay[which(season_Jday$z>=zc)[1]]
          
          if(sea==1){
            results<-season_data$Date[res$bloomindex] 
          }else{
            results<-c(results,season_data$Date[res$bloomindex])
          }
        }, silent = TRUE)  
      }
      
      # remove years of missing temperature data, if "Error in if (results_year[i] != seasons[i]) { : missing value where TRUE/FALSE needed" occurs, it can be ignored
      results_year = as.integer(format(results, format="%Y"))
      tryCatch(for (j in c(1:length(seasons))){
        if (results_year[j] != seasons[j]){
          seasons <- seasons[-c(j)]
        }else{
          seasons <- seasons
        }
      }, error = function(e) print("missing year"))
      
      predictions<-data.frame(Season=seasons,Prediction=results)
      
      predictions$Prediction<-ISOdate(2001,
                                      substr(predictions$Prediction,6,7),
                                      substr(predictions$Prediction,9,10))
      
      Observations$Prediction <- ISOdate(2001,
                                         substr(Observations$Date,4,5),
                                         substr(Observations$Date,1,2))
      
      # Plot
      plot_history <- ggplot() +
        geom_point(data=predictions,aes(x=Season,y=Prediction)) +
        geom_point(data = Observations, colour='red',aes(x=Year,y=Prediction))+
        geom_smooth(data=predictions,aes(x=Season,y=Prediction), colour = 'black', se=F, show.legend = TRUE) +
        geom_smooth(data= Observations,aes(x=Year,y=Prediction), colour = 'red',se=F, show.legend = TRUE) +              
        scale_color_discrete(name = "data",labels=c("Observations", "Model")) +
        ylab("Predicted bloom date") +
        theme_bw(base_size=15)
      plot_history <- plot_history + labs(title = paste("Bloom date",gsub("_"," ",applevar),i)) +
        theme(plot.title = element_text(hjust = 0.5))
      plot_history  
    }  
    
    figure <- ggarrange(plot_history, ncol = 1, nrow = 1)
    figure
    print(figure)
    dev.copy(png,paste(path,output,"Bloomdate_timeseries_model_obs_",applevar,"_",name,".png",sep=""),width=8,height=6,units="in",res=100)
    dev.off()
  }
  
########################################
## Bloom date multiple stations plots ##------------------------------------------------------------------------------------------
########################################
  
  if (3 == code[c]){
    print(paste("Bloom date plot for",station[1],station[2],station[3],"and",station[4]))
    Bloom_average <- data.frame(matrix(ncol = 2, nrow=4))
    colnames(Bloom_average) <-  c("Stations","Average bloomdate")
    
    for (i in 1:length(station)){
      hourtemps <- read.table(paste(path,HourlyTemp_folder,"hourly_temperature_",station[i],".txt", sep = ""), sep=",", header=1)
      hourtemps <- hourtemps[!is.na(hourtemps$Temp),] # remove NA temperature values
      
      #run model for the specified growing seasons
      #seasons<-years
      seasons <- (min(hourtemps$Year)+1):max(hourtemps$Year)
      
      iSeason <- genSeason(hourtemps,
                           mrange = c(8, 6),
                           years=seasons)
      
      #lists needed for damage indicator & chilling hours
      CR_full <- vector(mode='list', length=length(seasons))
      Bloom <- vector(mode='list', length=(length(seasons)))
      
      #run the model over all years previously specified
      for (sea in 1:(length(seasons))){
        try({
          season_data<-hourtemps[iSeason[[sea]],]
          
          res <- PhenoFlex(temp=season_data$Temp,
                           times=c(1: length(season_data$Temp)),
                           stopatzc=TRUE,
                           basic_output=FALSE,
                           deg_celsius = TRUE,
                           yc = par[1], zc = par[2], s1 = par[3],
                           Tu = par[4], E0 = par[5], E1 = par[6],
                           A0 = par[7], A1 = par[8], Tf = par[9],
                           Tc = par[10], Tb =par[11], slope = par[12])
          
          #Use Date + these paramteres + Temperature to make frost damage index
          season_data[,"x"]<-res$x
          season_data[,"y"]<-res$y
          season_data[,"z"]<-res$z
          season_data<-add_date(season_data)  
          season_Jday <-  make_JDay(season_data)
          
          CR_full[[sea]]<-season_Jday$JDay[which(season_Jday$y>=yc)[1]]
          Bloom[[sea]]<-season_Jday$JDay[which(season_Jday$z>=zc)[1]]
          if(sea==1){
            results<-season_data$Date[res$bloomindex]
          }else{
            results<-c(results,season_data$Date[res$bloomindex])
          }
          
        }, silent = TRUE)  
      }
      
      # remove years of missing temperature data, if "Error in if (results_year[i] != seasons[i]) { : missing value where TRUE/FALSE needed" occurs, it can be ignored
      results_year = as.integer(format(results, format="%Y"))
      tryCatch(for (j in c(1:length(seasons))){
        if (results_year[j] != seasons[j]){
          seasons <- seasons[-c(j)]
        }else{
          seasons <- seasons
        }
      }, error = function(e) print("missing year"))
      
      Bloom_average$Stations[i] <- station[i]
      Bloom_dataframe <- transpose(data.frame(Bloom))
      Bloom_average$`Average bloomdate`[i] <- mean(Bloom_dataframe$V1, na.rm = TRUE)
      
      
      predictions<-data.frame(Season=seasons,Prediction=results)
      
      predictions$Prediction<-ISOdate(2001,
                                      substr(predictions$Prediction,6,7),
                                      substr(predictions$Prediction,9,10))
      
      Observations$Prediction <- ISOdate(2001,
                                         substr(Observations$Date,4,5),
                                         substr(Observations$Date,1,2))
      
      # Plot
      plot_history <- ggplot() +
        geom_point(data=predictions,aes(x=Season,y=Prediction)) +
        geom_smooth(data=predictions,aes(x=Season,y=Prediction), colour = 'black', se=F, show.legend = TRUE) +           
        #scale_color_discrete(name = "data",labels=c("Observations", "model output"))+
        ylab("Predicted bloom date") +
        theme_bw(base_size=15) + ylim(ISOdate(2001,3,25),ISOdate(2001,5,30))
      plot_history <- plot_history + labs(title = station[i]) +
        theme(plot.title = element_text(hjust = 0.5))+
        annotate("text", x = (max(hourtemps$Year)-8), y = ISOdate(2001,5,25), label = paste("Average bloom DOY:\n",sprintf(Bloom_average[i,2], fmt = '%#.2f')  )  , size=3)
      plot_history  
      
      if (station[i] == station[1]){
        plot_history_1 <- plot_history
      }else if (station[i] == station[2]){
        plot_history_2 <- plot_history
      }else if (station[i] == station[3]){
        plot_history_3 <- plot_history
      }else if (station[i] == station[4]){
        plot_history_4 <- plot_history
      }
      
      
    }  
    
    figure <- ggarrange(plot_history_1, plot_history_2, plot_history_3, plot_history_4, ncol = 2, nrow = 2)
    figure <- annotate_figure(figure, top = text_grob(paste("Bloom date",gsub("_"," ",applevar)), color = "black", face = "bold", size = 18))
    figure
    print(figure)
    dev.copy(png,paste(path,output,"Bloomdate_timeseries_",applevar,"_",station[1],station[2],station[3],station[4],".png",sep=""),width=8,height=6,units="in",res=100)
    dev.off()
  }
  
############################################
## Chill & forcing for 1 year & 1 station ##--------------------------------------------------------------------------------------
############################################
  
  if (4 == code[c]){
    print(paste("Chilling & forcing plot for year",end_season_year,"for",name))
    
    CR_full <- vector(mode='list', length=length(name))
    Bloom <- vector(mode='list', length=(length(name)))
    Budburst <- vector(mode='list', length=length(name))
    Total_damage <- data.frame(matrix(ncol = 5, nrow=length(name)))
    colnames(Total_damage) <- c("Station","Damage_tot","Damage_1","Damage_10","Damage_90")
    
    for (i in 1:(length(name))){
      hourtemps <- read.table(paste(path,HourlyTemp_folder,"hourly_temperature_",name,".txt", sep = ""), sep=",", header=1)
      hourtemps <- hourtemps[!is.na(hourtemps$Temp),] # remove NA temperature values
      
      iSeason <- genSeason(hourtemps,
                           mrange = c(8, 6),
                           years=end_season_year)
      season_data<-hourtemps[iSeason[[1]],]
      
      res <- PhenoFlex(temp=season_data$Temp,
                       times=c(1: length(season_data$Temp)),
                       stopatzc=TRUE,
                       basic_output=FALSE,
                       deg_celsius = TRUE,
                       yc = par[1], zc = par[2], s1 = par[3],
                       Tu = par[4], E0 = par[5], E1 = par[6],
                       A0 = par[7], A1 = par[8], Tf = par[9],
                       Tc = par[10], Tb = par[11], slope = par[12])
      
      
      season_data[,"x"]<-res$x
      season_data[,"y"]<-res$y
      season_data[,"z"]<-res$z
      season_data<-add_date(season_data)  
      season_Jday <-  make_JDay(season_data)
      
      #make a seperate dataframe with the bloomdates
      results<-season_data$Date[res$bloomindex]
      
      #date chilling requirement is met (budburst)
      CR_full<-season_data$Date[which(season_data$y>=yc)[1]]
      
      #date heating requirement is met (bloom)
      #Bloom<-season_Jday$JDay[which(season_Jday$z>=zc)[1]]
      Bloom<-season_data$Date[which(season_data$z>=zc)[1]]
      DBreakDay <- res$bloomindex
      
      
      #frost index run thing here
      Damage_index_1 <- c()
      Damage_index_10 <- c()
      Damage_index_90 <- c()
      Damage_timing <- c()
      
      Stage = 0
      for (T in c(1:(nrow(season_Jday)))){
        frost_index_result <- frost_index(season_Jday$z[T], season_Jday$Temp[T], JDay = season_Jday$JDay[T], Bloom,T)
        if (frost_index_result == 1){
          Damage_index_1 <- append(Damage_index_1,frost_index_result)
          Damage_index_10 <- append(Damage_index_10,0)
          Damage_index_90 <- append(Damage_index_90,0)
          Damage_timing <- append(Damage_timing, season_Jday$Date[T])}
        else if (frost_index_result == 10){
          Damage_index_1 <- append(Damage_index_1,0)
          Damage_index_10 <- append(Damage_index_10,frost_index_result)
          Damage_index_90 <- append(Damage_index_90,0)
          Damage_timing <- append(Damage_timing, season_Jday$Date[T])}
        else if (frost_index_result == 100){
          Damage_index_1 <- append(Damage_index_1,0)
          Damage_index_10 <- append(Damage_index_10,0)
          Damage_index_90 <- append(Damage_index_90,frost_index_result)
          Damage_timing <- append(Damage_timing, season_Jday$Date[T])}
        else {
          Damage_index_1 <- append(Damage_index_1,0)
          Damage_index_10 <- append(Damage_index_10,0)
          Damage_index_90 <- append(Damage_index_90,0)
          Damage_timing <- append(Damage_timing, NULL)}
      }
      
      Damage_index_1_df <- as.data.frame(Damage_index_1)
      Damage_index_10_df <- as.data.frame(Damage_index_10)
      Damage_index_90_df <- as.data.frame(Damage_index_90)
      
      Total_damage$Damage_1[i] <- sum(Damage_index_1)
      Total_damage$Damage_10[i] <- sum(Damage_index_10)
      Total_damage$Damage_90[i] <- sum(Damage_index_90)
      Total_damage$Station[i] <- name
      Total_damage$Damage_tot[i] <- sum(Total_damage$Damage_1[i],Total_damage$Damage_10[i],Total_damage$Damage_90[i])
      
      season_Jday$Frost_damage_1 <- Damage_index_1_df
      season_Jday$Frost_damage_10 <- Damage_index_10_df
      season_Jday$Frost_damage_90 <- Damage_index_90_df
      
      
      chillplot<-ggplot(data=season_data[1:DBreakDay,],aes(x=Date,y=y)) +
        geom_line(col="blue",lwd=1.5) +
        theme_bw(base_size=20) +
        geom_hline(yintercept=yc,lty=2,col="blue",lwd=1.2) +
        geom_vline(xintercept=CR_full,lty=3,col="blue",lwd=1.2) +
        ylab("Chill accumulation") +
        labs(title="Chilling") +
        theme(plot.title = element_text(hjust = 0.5),text=element_text(size=10)) +
        annotate("text", x = ISOdate(end_season_year-1,10,15), y = 50, label = paste("Start growing season:\n", format(CR_full,format='%d.%m.')), size=4) +
        theme(text=element_text(size=13))
      # annotate("text",label="Chill req. (yc)", 
      #          x=ISOdate(2000,06,31), ##CHANGE DATE TO YEAR OF CHOICE
      #          y=yc*1.1, col="blue",size=5)
      
      #plotting heating hours
      heatplot<-ggplot(data=season_data[1:DBreakDay,],aes(x=Date,y=z)) +
        geom_line(col="red",lwd=1.5) +
        theme_bw(base_size=20) +
        geom_hline(yintercept=zc,lty=2,col="red",lwd=1.2) +
        geom_vline(xintercept=CR_full,lty=3,col="blue",lwd=1.2) +
        geom_vline(xintercept=Bloom,lty=3,col="red",lwd=1.2) +
        ylab("Heat accumulation") +
        labs(title="Forcing")  +
        theme(plot.title = element_text(hjust = 0.5),text=element_text(size=10)) +
        annotate("text", x = ISOdate(end_season_year-1,10,15), y = 150, label = paste("Flowering:\n", format(Bloom,format='%d.%m.')), size=4) +
        theme(text=element_text(size=13))
      # annotate("text",label="Heat req. (zc)", 
      #          x=ISOdate(2000,06,31), ##CHANGE DATE TO YEAR OF CHOICE
      #          y=zc*0.95, col="red",size=5)
      
      #display the plots
      chillplot + heatplot
      
      CP_1 <- chillplot
      HP_1 <- heatplot

    }
    
    figure_tot <- ggarrange(CP_1,HP_1, ncol = 2, nrow = 1)
    figure_tot <- annotate_figure(figure_tot, top = text_grob(paste("Chilling & Forcing",name,gsub("_"," ",applevar),end_season_year), color = "black", face = "bold", size = 18))
    figure_tot
    print(figure_tot)
    dev.copy(png,paste(path,output,"ChillForcing","_",applevar,"_",end_season_year,"_",name,".png",sep=""),width=8,height=5,units="in",res=100)
    dev.off()
  }

####################################################
## Chill & forcing for 1 year & multiple stations ##------------------------------------------------------------------------------
####################################################
  
  if (5 == code[c]){
    print(paste("Chilling & forcing plot for year",end_season_year,"for",station[1],station[2],station[3],"and",station[4]))
    
    CR_full <- vector(mode='list', length=length(station))
    Bloom <- vector(mode='list', length=(length(station)))
    Budburst <- vector(mode='list', length=length(station))
    Total_damage <- data.frame(matrix(ncol = 5, nrow=length(station)))
    colnames(Total_damage) <- c("Station","Damage_tot","Damage_1","Damage_10","Damage_90")
    
    for (i in 1:(length(station))){
      hourtemps <- read.table(paste(path,HourlyTemp_folder,"hourly_temperature_",station[i],".txt", sep = ""), sep=",", header=1)
      hourtemps <- hourtemps[!is.na(hourtemps$Temp),] # remove NA temperature values
      
      iSeason <- genSeason(hourtemps,
                           mrange = c(8, 6),
                           years=end_season_year)
      season_data<-hourtemps[iSeason[[1]],]
      
      res <- PhenoFlex(temp=season_data$Temp,
                       times=c(1: length(season_data$Temp)),
                       stopatzc=TRUE,
                       basic_output=FALSE,
                       deg_celsius = TRUE,
                       yc = par[1], zc = par[2], s1 = par[3],
                       Tu = par[4], E0 = par[5], E1 = par[6],
                       A0 = par[7], A1 = par[8], Tf = par[9],
                       Tc = par[10], Tb = par[11], slope = par[12])
      
      
      season_data[,"x"]<-res$x
      season_data[,"y"]<-res$y
      season_data[,"z"]<-res$z
      season_data<-add_date(season_data)  
      season_Jday <-  make_JDay(season_data)
      
      #make a seperate dataframe with the bloomdates
      results<-season_data$Date[res$bloomindex]
      
      #date chilling requirement is met (budburst)
      CR_full<-season_data$Date[which(season_data$y>=yc)[1]]
      
      #date heating requirement is met (bloom)
      #Bloom<-season_Jday$JDay[which(season_Jday$z>=zc)[1]]
      Bloom<-season_data$Date[which(season_data$z>=zc)[1]]
      DBreakDay <- res$bloomindex
      
      
      #frost index run thing here
      Damage_index_1 <- c()
      Damage_index_10 <- c()
      Damage_index_90 <- c()
      Damage_timing <- c()
      
      Stage = 0
      for (T in c(1:(nrow(season_Jday)))){
        frost_index_result <- frost_index(season_Jday$z[T], season_Jday$Temp[T], JDay = season_Jday$JDay[T], Bloom,T)
        if (frost_index_result == 1){
          Damage_index_1 <- append(Damage_index_1,frost_index_result)
          Damage_index_10 <- append(Damage_index_10,0)
          Damage_index_90 <- append(Damage_index_90,0)
          Damage_timing <- append(Damage_timing, season_Jday$Date[T])}
        else if (frost_index_result == 10){
          Damage_index_1 <- append(Damage_index_1,0)
          Damage_index_10 <- append(Damage_index_10,frost_index_result)
          Damage_index_90 <- append(Damage_index_90,0)
          Damage_timing <- append(Damage_timing, season_Jday$Date[T])}
        else if (frost_index_result == 100){
          Damage_index_1 <- append(Damage_index_1,0)
          Damage_index_10 <- append(Damage_index_10,0)
          Damage_index_90 <- append(Damage_index_90,frost_index_result)
          Damage_timing <- append(Damage_timing, season_Jday$Date[T])}
        else {
          Damage_index_1 <- append(Damage_index_1,0)
          Damage_index_10 <- append(Damage_index_10,0)
          Damage_index_90 <- append(Damage_index_90,0)
          Damage_timing <- append(Damage_timing, NULL)}
      }
      
      Damage_index_1_df <- as.data.frame(Damage_index_1)
      Damage_index_10_df <- as.data.frame(Damage_index_10)
      Damage_index_90_df <- as.data.frame(Damage_index_90)
      
      Total_damage$Damage_1[i] <- sum(Damage_index_1)
      Total_damage$Damage_10[i] <- sum(Damage_index_10)
      Total_damage$Damage_90[i] <- sum(Damage_index_90)
      Total_damage$Station[i] <- station[i]
      Total_damage$Damage_tot[i] <- sum(Total_damage$Damage_1[i],Total_damage$Damage_10[i],Total_damage$Damage_90[i])
      
      season_Jday$Frost_damage_1 <- Damage_index_1_df
      season_Jday$Frost_damage_10 <- Damage_index_10_df
      season_Jday$Frost_damage_90 <- Damage_index_90_df
      
      
      chillplot<-ggplot(data=season_data[1:DBreakDay,],aes(x=Date,y=y)) +
        geom_line(col="blue",lwd=1.5) +
        theme_bw(base_size=20) +
        geom_hline(yintercept=yc,lty=2,col="blue",lwd=1.2) +
        geom_vline(xintercept=CR_full,lty=3,col="blue",lwd=1.2) +
        ylab("Chill accumulation") +
        labs(title="Chilling") +
        theme(plot.title = element_text(hjust = 0.5),text=element_text(size=10)) +
        annotate("text", x = ISOdate(end_season_year-1,10,15), y = 50, label = paste("Start growing season:\n", format(CR_full,format='%d.%m.')), size=3)
      # annotate("text",label="Chill req. (yc)", 
      #          x=ISOdate(2000,06,31), ##CHANGE DATE TO YEAR OF CHOICE
      #          y=yc*1.1, col="blue",size=5)
      
      #plotting heating hours
      heatplot<-ggplot(data=season_data[1:DBreakDay,],aes(x=Date,y=z)) +
        geom_line(col="red",lwd=1.5) +
        theme_bw(base_size=20) +
        geom_hline(yintercept=zc,lty=2,col="red",lwd=1.2) +
        geom_vline(xintercept=CR_full,lty=3,col="blue",lwd=1.2) +
        geom_vline(xintercept=Bloom,lty=3,col="red",lwd=1.2) +
        ylab("Heat accumulation") +
        labs(title="Forcing")  +
        theme(plot.title = element_text(hjust = 0.5),text=element_text(size=10)) +
        annotate("text", x = ISOdate(end_season_year-1,10,15), y = 150, label = paste("Flowering:\n", format(Bloom,format='%d.%m.')), size=3)
      # annotate("text",label="Heat req. (zc)", 
      #          x=ISOdate(2000,06,31), ##CHANGE DATE TO YEAR OF CHOICE
      #          y=zc*0.95, col="red",size=5)
      
      #display the plots
      chillplot + heatplot
      
      if (station[i] == station[1]){
        CP_1 <- chillplot
        HP_1 <- heatplot
      }else if (station[i] == station[2]){
        CP_2 <- chillplot
        HP_2 <- heatplot
      }else if (station[i] == station[3]){
        CP_3 <- chillplot
        HP_3 <- heatplot
      }else if (station[i] == station[4]){
        CP_4 <- chillplot
        HP_4 <- heatplot
      }
    }
    figure1 <- ggarrange(CP_1,CP_2,CP_3,CP_4,ncol = 4, nrow = 1, labels = station, font.label=list(size=9))
    figure2 <- ggarrange(HP_1,HP_2,HP_3,HP_4,ncol = 4, nrow = 1, labels = station, font.label=list(size=9))
    
    figure_tot <- ggarrange(figure1,figure2, ncol = 1, nrow = 2)
    figure_tot <- annotate_figure(figure_tot, top = text_grob(paste("Chilling & Forcing",gsub("_"," ",applevar),end_season_year), color = "black", face = "bold", size = 18))
    figure_tot
    print(figure_tot)
    dev.copy(png,paste(path,output,"ChillForcing","_",applevar,"_",end_season_year,"_",station[1],station[2],station[3],station[4],".png",sep=""),width=8,height=6,units="in",res=100)
    dev.off()
  }
  
##################################
## Create files of model output ##------------------------------------------------------------------------------------------------
##################################
  
  if (6 == code[c]){
  print(paste("Create files of model output for..."))
    for (i in station_file){
      print(i)
      hourtemps <- read.table(paste(path,HourlyTemp_folder,"hourly_temperature_",i,".txt", sep = ""), sep=",", header=1)
      hourtemps <- hourtemps[!is.na(hourtemps$Temp),] # remove NA temperature values
      
      #run model for the specified growing seasons
      # if (max(hourtemps$Year) < 2022){
      #   seasons <- (min(hourtemps$Year)+1):max(hourtemps$Year)
      # }else{
      #   seasons <- (min(hourtemps$Year)+1):(max(hourtemps$Year)-1)
      # }
      
      seasons <- (min(hourtemps$Year)):max(hourtemps$Year)
      
      iSeason <- genSeason(hourtemps,
                           mrange = c(8, 6),
                           years=seasons)
      
      #lists needed for damage indicator & chilling hours
      CR_full <- vector(mode='list', length=length(seasons))
      Bloom <- vector(mode='list', length=(length(seasons)))
      Budburst <- vector(mode='list', length=length(seasons))
      Total_damage <- data.frame(matrix(ncol = 2, nrow=length(seasons)))
      colnames(Total_damage) <- c("Year","Damage")
      
      #run the model over all years previously specified
      
      for (sea in 1:(length(seasons))){
        try({
        season_data<-hourtemps[iSeason[[sea]],]
        
        res <- PhenoFlex(temp=season_data$Temp,
                         times=c(1: length(season_data$Temp)),
                         stopatzc=TRUE,
                         basic_output=FALSE,
                         deg_celsius = TRUE,
                         yc = par[1], zc = par[2], s1 = par[3],
                         Tu = par[4], E0 = par[5], E1 = par[6],
                         A0 = par[7], A1 = par[8], Tf = par[9],
                         Tc = par[10], Tb =par[11], slope = par[12])
        
        #Use Date + these paramteres + Temperature to make frost damage index
        season_data[,"x"]<-res$x
        season_data[,"y"]<-res$y
        season_data[,"z"]<-res$z
        season_data<-add_date(season_data)  
        season_Jday <-  make_JDay(season_data)
        
        CR_full[[sea]]<-season_Jday$JDay[which(season_Jday$y>=yc)[1]]
        Bloom[[sea]]<-season_Jday$JDay[which(season_Jday$z>=zc)[1]]
        Budburst[[sea]]<-season_Jday$JDay[which(season_Jday$z>=(0.19*zc))[1]]
        
        #frost index run thing here
        Damage_index_1 <- c()
        Damage_index_10 <- c()
        Damage_index_90 <- c()
        Damage_timing <- c()
        
        Stage = 0
        for (T in c(1:(nrow(season_Jday)))){
          frost_index_result <- frost_index(season_Jday$z[T], season_Jday$Temp[T], JDay = season_Jday$JDay[T], Bloom[[sea]],T)
          if (frost_index_result == 1){
            Damage_index_1 <- append(Damage_index_1,frost_index_result)
            Damage_index_10 <- append(Damage_index_10,0)
            Damage_index_90 <- append(Damage_index_90,0)
            Damage_timing <- append(Damage_timing, season_Jday$Date[T])}
          else if (frost_index_result == 10){
            Damage_index_1 <- append(Damage_index_1,0)
            Damage_index_10 <- append(Damage_index_10,frost_index_result)
            Damage_index_90 <- append(Damage_index_90,0)
            Damage_timing <- append(Damage_timing, season_Jday$Date[T])}
          else if (frost_index_result == 100){
            Damage_index_1 <- append(Damage_index_1,0)
            Damage_index_10 <- append(Damage_index_10,0)
            Damage_index_90 <- append(Damage_index_90,frost_index_result)
            Damage_timing <- append(Damage_timing, season_Jday$Date[T])}
          else {
            Damage_index_1 <- append(Damage_index_1,0)
            Damage_index_10 <- append(Damage_index_10,0)
            Damage_index_90 <- append(Damage_index_90,0)
            Damage_timing <- append(Damage_timing, NULL)}
        }
        
        Damage_index_1_df <- as.data.frame(Damage_index_1)
        Damage_index_10_df <- as.data.frame(Damage_index_10)
        Damage_index_90_df <- as.data.frame(Damage_index_90)
        
        Total_damage$Damage_1[sea] <- sum(Damage_index_1)
        Total_damage$Damage_10[sea] <- sum(Damage_index_10)
        Total_damage$Damage_90[sea] <- sum(Damage_index_90)
        
        #  if (is.null(Damage_timing) == FALSE){
        Filter(Negate(is.null), Damage_timing)
        #}
        as.Date(Damage_timing)
        Total_damage$Timing[sea] <- toString(Damage_timing)
        
        
        Total_damage$Year[sea] <- seasons[sea]
        #  if (sea == 1){Total_damage$Year[sea] <- (1951)}
        #  else{Total_damage$Year[sea] <- (Total_damage$Year[sea -1] + 2)
        #  }
        
        season_Jday$Frost_damage_1 <- Damage_index_1_df
        season_Jday$Frost_damage_10 <- Damage_index_10_df
        season_Jday$Frost_damage_90 <- Damage_index_90_df
        }, silent = TRUE)   
      }
      
      for (sea in 1:(length(seasons))){
        if (is.null(CR_full[[sea]]) == TRUE){CR_full[[sea]] <- NA}
        if (is.null(Bloom[[sea]]) == TRUE){Bloom[[sea]] <- NA}
        if (is.null(Budburst[[sea]]) == TRUE){Budburst[[sea]] <- NA}
      }
      CR_full1 <- as.data.frame(do.call(rbind,CR_full))
      Bloom1 <- as.data.frame(do.call(rbind,Bloom))
      Budburst1 <- as.data.frame(do.call(rbind,Budburst))
      
      All_outputs <- data.frame(matrix(ncol = 8, nrow = length(seasons)))
      colnames(All_outputs) <- c("Year","Stop chilling DOY","Budburst DOY" ,"Bloom DOY","Mild Frost Damage", "Moderate Frost Damage","Severe Frost Damage", "Damage Timing")
      All_outputs$Year <- Total_damage$Year
      All_outputs$`Stop chilling DOY` <- CR_full1$V1
      All_outputs$`Budburst DOY` <- Budburst1$V1
      All_outputs$`Bloom DOY` <- Bloom1$V1
      All_outputs$`Mild Frost Damage` <- Total_damage$Damage_1
      All_outputs$`Moderate Frost Damage` <- Total_damage$Damage_10
      All_outputs$`Severe Frost Damage` <- Total_damage$Damage_90
      All_outputs$`Damage Timing` <- Total_damage$Timing
      write.table(All_outputs,paste(path,output,"Data_",applevar,"_",i,".txt", sep=""),sep=",",row.names=FALSE)
    }  
  }
  
#############################################
## RMSE of model & plot model against obs. ##-------------------------------------------------------------------------------------
#############################################
  
  if (7 == code[c]){
  print(paste("Calculate RMSE and plot model against observation for",name_RMSE))
    for (i in name_RMSE){
      print(i)
      hourtemps <- read.table(paste(path,HourlyTemp_folder,"hourly_temperature_",i,".txt", sep = ""), sep=",", header=1)
      
      #run model for the specified growing seasons
      seasons <- years
      
      seasons_sel <- c()
      count = 1 # maintaining counter 
      # select every second year
      for (l in seasons){
        #count= count + 1
        if(count %% n2 == 0){
          seasons_sel <- append(seasons_sel,l)
        }  
        count= count + 1
      }  
      
      iSeason <- genSeason(hourtemps,
                           mrange = c(8, 6),
                           years=seasons_sel)
      
      #lists needed for damage indicator & chilling hours
      Bloom <- vector(mode='list', length=(length(seasons_sel)))
      
      #run the model over all years previously specified
      
      for (sea in 1:(length(seasons_sel))){
        season_data<-hourtemps[iSeason[[sea]],]
        
        res <- PhenoFlex(temp=season_data$Temp,
                         times=c(1: length(season_data$Temp)),
                         stopatzc=TRUE,
                         basic_output=FALSE,
                         deg_celsius = TRUE,
                         yc = par[1], zc = par[2], s1 = par[3],
                         Tu = par[4], E0 = par[5], E1 = par[6],
                         A0 = par[7], A1 = par[8], Tf = par[9],
                         Tc = par[10], Tb =par[11], slope = par[12])
        
        #Use Date + these paramteres + Temperature to make frost damage index
        season_data[,"x"]<-res$x
        season_data[,"y"]<-res$y
        season_data[,"z"]<-res$z
        season_data<-add_date(season_data)  
        season_Jday <- make_JDay(season_data)
        Bloom[[sea]] <- season_Jday$JDay[which(season_Jday$z>=zc)[1]]
      }
      
      JDay <- c()
      for (j in years){ 
        JDay <- append(JDay,mean(Observations$JDay[which(Observations$Year == j)]))
      }
      
      JDay_sel <- c()
      count = 1 # maintaining counter 
      # select every second year
      for (k in JDay){
        if(count %% n2 == 0){
          JDay_sel <- append(JDay_sel,k)
        }  
        count= count + 1
      }
      
      Observation_sel <- data.frame(JDay_sel, seasons_sel)
      Bloom_dataframe <- transpose(data.frame(Bloom))
      
      validation_df <-data.frame(Observation_sel$JDay_sel, Bloom_dataframe$V1)
      
      plot_val <- ggplot(validation_df,aes(Observation_sel.JDay_sel, Bloom_dataframe.V1)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1) +
        labs(x = "Observed Bloom Date [DOY]", y = "Model Bloom Date [DOY]") +
        xlim(95,140) + ylim(95,140) +
        theme_bw()
      plot_val
      print(plot_val)
      dev.copy(png,paste(path,output,"Model_against_obs_",applevar,"_",name_RMSE,".png",sep=""),width=8,height=6,units="in",res=100)
      dev.off()
      
      RMSE_model <- RMSEP(Bloom_dataframe$V1, Observation_sel$JDay_sel, na.rm = TRUE)
      print(RMSE_model)
    }
  }
}
############################################################################################
##### END MODEL ############################################################################
############################################################################################














#################
#make table of Phenological data(is not used in the model)----------------------------------------------------------
#################

# for (i in 1){
# 
# #load extra phenology data
# Phenology_data <- read_excel(paste(path,"Data_phenological stages.xlsx",sep=""))
# Phenology_data_df <- as.data.frame(Phenology_data)
# 
# #adapt loop to suite species
# Phenology_data_df_doy <- data.frame(matrix(ncol = 13, nrow = 12))
# 
# for(a in 1:12) {       # for-loop over rows
#   
#   for(b in 6:19) {       # for-loop over columns
#     
#     if (is.na(Phenology_data_df[a , b])){
#       print (a)
#       print (b)
#     }
#     else{
#       print (a)
#       print (b)
#       Phenology_data_df_doy[a , b]<- yday(Phenology_data_df[a , b] )
#       
#       print("jeej!")
#     }
#   }
# }
# Phenology_data_df_doy <- subset (Phenology_data_df_doy, select = -c(X1:X5))
# 
# for (b in 1:14){
#   Phenology_data_df_doy[[13,b]] <- mean(Phenology_data_df_doy[,b],na.rm = TRUE)}
# 
# #make empty matrix for GDH things
# GDH_pheo_stages <- data.frame(matrix(ncol = 14, nrow = 13))
# 
# }
