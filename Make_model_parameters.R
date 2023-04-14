#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# !! READ.me !!
#
# FSDI model make parameters V1.1
# Authors: Joep Bosdijk (joepbosdijk@outlook.com)
# Group members: Joep Bosdijk, Annika Gaiser, Vincent de Feiter, Gudrun Torkelsdottir, Thijs Smink
#
# This script can create the following output:
# - model parameters for different plant species/apple varieties
#
# The following input files are required:
# - hourly temperature of meteorological stations; downloaded from KNMI and adapted via the script "TransformKNMItoFile.R" (filename automatically done) (structure: "Year"[int],"Month"[int],"Day"[int],"Hour"[int],"Temp"[num])
# - observational phenology data (bloom dates) (filename: Applevariety_possiblefilenameadditions.txt) (structure: Year[int],JDay[int],Date[chr, "dd-mm"],DD-MM-YYYY[chr, "dd-m(m)-yyyy"])
#
# How to use the model:
# - adapt below the input values, run then the complete skript
#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# load required libraries; if error is given, the libraries need to be installed
library(readr)
library(tidyr)
library(chillR)
library(ggplot2)
library(patchwork)
require(reshape2)

rm(list=ls()) # remove variable names in environment; recommended before starting a new run


###################################################################################################################
##### CHANGE BELOW: paths, folders, station name, apple variety, possibly file addition for the apple variety #####
###################################################################################################################

# define main path & folder paths (if folder directory same as main path, just use empty "")
path <- "C:\\FSDI_model\\" # path to the main folder
HourlyTemp_folder <- "hourly_data\\" # temperature data
pheno_folder <- "phenological data\\" # apple observation files 
model_param_folder <- "Parameters\\" # model parameters

# specify station name
name <- c("DeBilt") # DEFAULT

# specify apple variety
applevar <- "James_Grieve" # Boskoop, Cox, Golden_Delicious, James_Grieve
applevar_filenameaddition <- "_filtered_stations_2000" # change year 2010, 2004, 2015, 2000

#################################################################################################
##### END CHANGES ###############################################################################
#################################################################################################


# If more observational phenology data is available ll.75-82 need to be adapted


#################################################################################################
##### Create model parameters ###################################################################
#################################################################################################

for (i in c(1:length(name))){
  try({
    stationname <- name[i] # for input txt file
    print(name[i])
    
    #load temperature data
    hourtemps <- read.table(paste(path,HourlyTemp_folder,"hourly_temperature_",stationname,".txt", sep=""), sep=",", header=1)
    
    #load the phenological data
    appledata <- read.table(paste(path,pheno_folder,applevar,applevar_filenameaddition,".txt",sep=""), sep=",", header=1)
    
    # define JDay
    JDay <- appledata$JDay
    
    #define JDay & years available in data
    if (applevar == "Boskoop"){
      years = c(1958:2000)
    }else if (applevar == "Cox"){
      years = c(1958:2004)
    }else if (applevar == "Golden_Delicious"){
      years <-  append(c(1958:2012),c(2014:2015))
    }else if (applevar == "James_Grieve"){
      years <- c(1958,2000)
    }
  
    
    #Boskoop_year <- substr(Boskoop_bloom$DD.MM.YYYY, nchar(Boskoop_bloom$DD.MM.YYYY) - 4 + 1, nchar(Boskoop_bloom$DD.MM.YYYY))
    #Boskoop_bloom[,"Year"] <- strtoi(year)
    #Boskoop_bloom[,"Month"]<-substr(Boskoop_bloom$Date,5,5)
    #Boskoop_bloom[,"Day"]<-substr(Boskoop_bloom$Date,1,2)
    #Boskoop_bloom <- make_JDay(Boskoop_bloom)
    
    JDay_sel <- c()
    n = 2
    count = 0 # maintaining counter 
    # select every second year
    for (k in JDay){
      if(count %% n == 0){
        JDay_sel <- append(JDay_sel,k)
      }  
      count= count + 1
    }
    
    years_sel <- c()
    n = 2
    count = 0 # maintaining counter 
    # select every second year
    for (l in years){
      #count= count + 1
      if(count %% n == 0){
        years_sel <- append(years_sel,l)
      }  
      
      count= count + 1
    }
    #print(years_sel)
    
    #hourtemps_sel <- hourtemps[hourtemps$Year %in% c(min(years_sel)-1,years_sel),]
    #hourtemps_sel <- make_JDay(hourtemps_sel)
    hourtemps <- make_JDay(hourtemps)
    #hourtemps <- na.omit(hourtemps)
    SeasonList_sel <- genSeasonList(hourtemps, mrange = c(8, 6), years=years_sel) 
    #SeasonList <- genSeasonList(hourtemps, mrange = c(8, 6), years=years) 
    #print(SeasonList_sel)
    
    #minJDay_sel <- SeasonList_sel[[1]]$JDay[1]
    #print(SeasonList_sel[[1]])
    #maxJDay_sel <- SeasonList_sel[[1]]$JDay[length(SeasonList_sel[[1]]$JDay)]
    
    
    #hourtemps <- make_JDay(hourtemps)
    #SeasonList <- genSeasonList(hourtemps, mrange = c(8, 6), years=years) 
    
    
    # here's the order of the parameters (from the helpfile of the
    # PhenoFlex_GDHwrapper function)
    #          yc,  zc,  s1, Tu,    E0,      E1,     A0,         A1,   Tf, Tc, Tb,  slope
    par <-   c(40, 190, 0.5, 25, 3372.8,  9900.3, 6319.5, 5.939917e13,  4, 36,  4,  1.60)
    upper <- c(41, 200, 1.0, 30, 4000.0, 10000.0, 7000.0,       6.e13, 10, 40, 10, 50.00)
    lower <- c(38, 180, 0.1, 0 , 3000.0,  9000.0, 6000.0,       5.e13,  0,  0,  0,  0.05)
    
    #run the paramterer fitter and model at the same time
    Fit_res <- phenologyFitter(par.guess=par, 
                               modelfn = PhenoFlex_GDHwrapper,
                               bloomJDays=JDay_sel, 
                               SeasonList=SeasonList_sel,
                               lower=lower,
                               upper=upper,
                               control=list(smooth=FALSE, verbose=FALSE, maxit=1000,
                                            nb.stop.improvement=5))
    #save parameters in csv
    par<-Fit_res$par
    write.csv(par,paste(path,model_param_folder,"PhenoFlex_parameters_",applevar,".csv", sep=""))
  },silent = TRUE)
}
