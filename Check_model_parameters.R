#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# !! READ.me !!
#
# FSDI model parameter check V1.1
# Author: Joep Bosdijk (joepbosdijk@outlook.com)
# Group members: Joep Bosdijk, Annika Gaiser, Vincent de Feiter, Gudrun Torkelsdottir, Thijs Smink
#
# This scripts can create the following output:
# - plots of the chill and heat respont curves, which can be used to check whether the parameters created in "Make_model_parameters.R" are reasonable
#
# The following input files are required:
# - hourly temperature of meteorological stations; downloaded from KNMI and adapted via the script "TransformKNMItoFile.R" (filename automatically done) (structure: "Year"[int],"Month"[int],"Day"[int],"Hour"[int],"Temp"[num])
# - species/variety specific model parameters, created with the script "Make_model_parameters.R" (filename automatically done)
# - observational phenology data (bloom dates) (filename: Applevariety_possiblefilenameadditions.txt) (structure: Year[int],JDay[int],Date[chr, "dd-mm"],DD-MM-YYYY[chr, "dd-m-yyyy"])
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


##################################################################################################################
##### CHANGE BELOW: paths, folders, station name, apple variety ##################################################
##################################################################################################################

# define main path & folder paths (if folder directory same as main path, just use empty "")
path <- "C:\\FSDI_model\\" # path to the main folder
HourlyTemp_folder <- "hourly_data\\" # temperature data
pheno_folder <- "phenological data\\" # apple observation files 
model_param_folder <- "Parameters\\" # model parameters

# define station name
stationname <- "DeBilt" # DEFAULT

# define apple variety 
applevar <- "Cox" # Boskoop, Cox, Golden_Delicious, James_Grieve

##################################################################################################################
##### END CHANGES ################################################################################################
##################################################################################################################


#load temperature data
hourtemps <- read.table(paste(path,HourlyTemp_folder,"hourly_temperature_",stationname,".txt", sep = ""), sep=",", header=1)

#parameters
par <- read_tab(paste(path,model_param_folder,"PhenoFlex_parameters_",applevar,".csv", sep=""))[,2]

yc = par[1]
zc = par[2]
s1 = par[3]
Tu = par[4]
E0 = par[5]
E1 = par[6]
A0 = par[7]
A1 = par[8]
Tf = par[9]
Tc = par[10]
Tb = par[11]
slope = par[12]


####################################
#Check chill + heat response curves#--------------------------------------------------------------------------------
####################################

apply_const_temp <- function(temp, A0, A1, E0, E1, Tf, slope, portions=1200, deg_celsius=TRUE){
  temp_vector <- rep(temp, times=portions)
  res <- chillR::DynModel_driver(temp=temp_vector,
                                 A0=A0, A1=A1,
                                 E0=E0, E1=E1,
                                 Tf=Tf,
                                 slope=slope,
                                 deg_celsius=deg_celsius)
  return(res$y[length(res$y)])
}

#test <- function(x,y)
#{number <- c(x-y,x+y)
#return(number)}

#produces the chill effectiveness curve

gen_bell <- function(par, temp_values=seq(-5, 30, 0.1)) {
  E0 <- par[5]
  E1 <- par[6]
  A0 <- par[7]
  A1 <- par[8]
  Tf <- par[9]
  slope <- par[12]
  
  y <- c()
  for(i in seq_along(temp_values)) {
    y[i] <- apply_const_temp(temp=temp_values[i],
                             A0=A0, A1=A1, E0=E0, E1=E1, Tf=Tf, slope=slope, portions=1200, deg_celsius=TRUE)
  }
  return(invisible(y))
}

#illustrates heat effectiveness
GDH_response<-function(T, par)
{Tb<-par[11]
Tu<-par[4]
Tc<-par[10]
GDH_weight <- rep(0, length(T))
GDH_weight[which(T >= Tb & T <= Tu)] <-
  1/2 * (1 + cos(pi + pi * (T[which(T >= Tb & T <= Tu)] - Tb)/(Tu - Tb)))
GDH_weight[which(T > Tu & T <= Tc)] <-
  (1 + cos(pi/2 + pi/2 * (T[which(T >  Tu & T <= Tc)] -Tu)/(Tc - Tu)))
return(GDH_weight)
}



temp_values=seq(-30, 30, 0.1)

temp_response<-data.frame(Temperature=temp_values,
                          Chill_response=gen_bell(par, temp_values),
                          Heat_response=GDH_response(temp_values,par))

melted_response<-melt(temp_response,id.vars="Temperature")


ggplot(melted_response,aes(x=Temperature,y=value)) +
  geom_line(size=2,aes(col=variable)) +
  ylab("Temperature response (arbitrary units)") +
  xlab("Temperature (°C)") +
  facet_wrap(vars(variable),scales="free",
             labeller = labeller(variable=c(Chill_response=c("Chill response"),
                                            Heat_response=c("Heat response")))) +
  scale_color_manual(values = c("Chill_response" = "blue", "Heat_response" = "red")) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")

 
#####################
#End response curves------------------------------------------------------------------------------------------------
####################
