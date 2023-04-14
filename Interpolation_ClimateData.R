#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# !! READ.me !!
#
# FSDI model interpolation temperature climate data V1.1
# Author: Annika Gaiser (annika.gaiser@web.de)
# Group members: Joep Bosdijk, Annika Gaiser, Vincent de Feiter, Gudrun Torkelsdottir, Thijs Smink
#
# This scripts creates hourly temperature data for climate scenarios 
# - daily minimum and maximum temperatures are interpolated to an hourly temperature time series 
#
# The following input files are required:
# - hourly temperature of meteorological stations for different climate change scenarios (filename: Tmax_ClimateScenario2050_GH.txt -> Tmax/Tmin, 2050/2085, GH/GL/WL/WH)
#
# How to use the script:
# - Adapt below the input values, run then the complete script
#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# load required libraries; if error is given, the libraries need to be installed
library(xts)
library(readr)
library(tidyr)
library(chillR)
library(ggplot2)
library(patchwork)
require(reshape2)
library(ggpubr)
require(grid)

rm(list=ls())

################################################################################################
##### CHANGE BELOW: paths, folders, station name, year scenarios, climate change scenarios #####
################################################################################################

## define folder paths (if folder directory same as main path, just use empty "")
path <- "C:\\FSDI_model\\" # path to the main folder
KNMI_data <- "KNMI_data\\" # folder of downloaded KNMI data:  Tmax & Tmin files (input)
pheno_folder <- "Data - Nature Today\\" # apple observation files (input)
model_param_folder <- "Data - Nature Today\\Model parameters\\" # model parameter files (input)
ClimateScenario_folder <- "Climate_change_scenario_data\\" # created hourly temperature files (output)

# define the station
stationname <- "DeBilt" #Valkenburg	DeKooy	Schiphol	DeBilt	Leeuwarden	Deelen	Eelde	Twenthe	Vlissingen	Rotterdam	GilzeRijen	Eindhoven	Volkel	Maastricht

# define climate change scenarios
years_scen <- c(2050,2085)
cc_scen <- c("GL","GH","WL","WH") # default

# The following changes might be necessary below the "End Changes":
# line 67 change station name if other station than De Bilt used
# if other year scenarios than 2050 and 2085, add it after if-else-statement in ll.70-74
# line 86 change station specific latitude if other station than De Bilt used

#################################################################################################
##### END CHANGES ###############################################################################
#################################################################################################


# Start loop
for (scenario in years_scen){
  for (cc in cc_scen) {
    Tmin <- read.table(paste(path,KNMI_data,"Tmin_ClimateScenario",scenario,"_",cc,".txt", sep = ""), sep="\t", header=1)
    Tmax <- read.table(paste(path,KNMI_data,"Tmax_ClimateScenario",scenario,"_",cc,".txt", sep = ""), sep="\t", header=1)

    splitted <- t(sapply(Tmin$Date, function(x) substring(x, first=c(1,5,7), last=c(4,6,8))))
    T_total <- cbind(splitted, Tmin$DeBilt, Tmax$DeBilt) # change station here 2x  #Valkenburg	DeKooy	Schiphol	DeBilt	Leeuwarden	Deelen	Eelde	Twenthe	Vlissingen	Rotterdam	GilzeRijen	Eindhoven	Volkel	Maastricht
    T_df <- as.data.frame(T_total)
    colnames(T_df) <- c('Year','Month','Day','Tmin','Tmax')
    T_df$Year <- as.integer(T_df$Year)
    
    # switch from input years to output years (consider LEAP years!!)
    if (scenario == 2085){
      T_df$Year <- T_df$Year+88
    }else if (scenario == 2050){
      T_df$Year <- T_df$Year+56
    }
    
    
    T_df$Month <- as.integer(T_df$Month)
    T_df$Day <- as.integer(T_df$Day)
    T_df$Tmin <- as.numeric(T_df$Tmin)
    T_df$Tmax <- as.numeric(T_df$Tmax)
    
    # adapt latitude
    T_hourly <- stack_hourly_temps(T_df, latitude=52.1)
    T_hourly_df <- T_hourly[["hourtemps"]]
    
    Df_temp <- data.frame(as.character(T_hourly_df$Year),as.character(T_hourly_df$Month),as.character(T_hourly_df$Day),T_hourly_df$Hour+1,T_hourly_df$Temp)
    colnames(Df_temp) <- c('Year','Month','Day','Hour','Temp')
    
    write.table(Df_temp,paste(path,ClimateScenario_folder,"ClimateScenario",scenario,"_",cc,"_",stationname,".txt", sep=""),sep=",",row.names=FALSE)
    
  }
}

