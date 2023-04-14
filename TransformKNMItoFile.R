#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# !! READ.me !!
#
# FSDI model KNMI Data Adaptation V1.1
# Authors: Annika Gaiser (annika.gaiser@web.de)
# Group members: Joep Bosdijk, Annika Gaiser, Vincent de Feiter, Gudrun Torkelsdottir, Thijs Smink
#
# This script can create the following outputs:
# - file of hourly temperature data to use for model (structure: "Year"[int],"Month"[int],"Day"[int],"Hour"[int],"Temp"[num])
#
# The following input files are required:
# - hourly temperature, one file for each meteorological station, downloaded from KNMI (filename: stationname.txt)
#   -> the following manual adaptations need to be done:
#       - remove the "#" in front of the column names
#       - remove the empty line between column names and first values
#
# How to use the model:
# - adapt below the available station names, run then the complete skript
#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# load required libraries; if error is given, the libraries need to be installed
library(data.table)

rm(list=ls()) # remove variable names in environment; recommended before starting a new run

########################################################################################################
##### CHANGE BELOW: paths, folders, station names ######################################################
########################################################################################################

## define folder paths (if folder directory same as main path, just use empty "")
path <- "C:\\FSDI_model\\" # path to the main folder
KNMI_data <- "KNMI_data\\" # input temperature files
HourlyTemp_folder <- "hourly_data\\" # temperature data

#----------------------------------------------------------------------------------
# name1: as written in downloaded KNMI files used as input; name2: without spaces & special symbols -> used in output file
name1 <- c("Arcen","Berkhout","Cabauw","De Bilt","De Kooy","Deelen","Eelde","Eindhoven","Ell","Gilze-Rijen","Heino","Herwijnen","Hoek van Holland","Hoogeveen","Hupsel","IJmuiden","Lauwersoog","Leeuwarden","Maastricht","Marknesse","Nieuw Beerta","R'dam-Geulhaven","Rotterdam","Schiphol","Soesterberg","Stavoren","Terschelling","Twenthe","ValkenburgZH","Vlieland","Vlissingen","Volkel","Voorschoten","Westdorpe","Wijk aan Zee","Wilhelminadorp","Woensdrecht")
name2 <- c("Arcen","Berkhout","Cabauw","DeBilt","DeKooy","Deelen","Eelde","Eindhoven","Ell","GilzeRijen","Heino","Herwijnen","HoekvanHolland","Hoogeveen","Hupsel","IJmuiden","Lauwersoog","Leeuwarden","Maastricht","Marknesse","NieuwBeerta","RdamGeulhaven","Rotterdam","Schiphol","Soesterberg","Stavoren","Terschelling","Twenthe","ValkenburgZH","Vlieland","Vlissingen","Volkel","Voorschoten","Westdorpe","WijkaanZee","Wilhelminadorp","Woensdrecht")

########################################################################################################
##### END CHANGES ######################################################################################
########################################################################################################



# START file creation... if error, see line 52
for (i in c(1:length(name1))){
  stationname <- name1[i] # for input txt file
  stationname2 <- name2[i] # for output txt file 
  
  # if you get an error in the following line, copy in the input txt file the last line that should not be included and replace the given one after skip =
  station <- fread(paste(path,KNMI_data,stationname,".txt", sep=""), skip="Y         = IJsvorming 0=niet voorgekomen, 1=wel voorgekomen in het voorgaande uur en/of tijdens de waarneming / Ice formation 0=no occurrence, 1=occurred during the preceding hour and/or at the time of observation", header=T)
  splitted <- t(sapply(station$YYYYMMDD, function(x) substring(x, first=c(1,5,7), last=c(4,6,8))))
  station_new <- cbind(station,splitted)
  station_txt <- data.frame(station_new$V1,station_new$V2,station_new$V3,station$HH,station$T)
  colnames(station_txt) <- c('Year','Month','Day','Hour','Temp')
  station_txt$Temp <- station_txt$Temp/10
  
  write.table(station_txt,paste(path,HourlyTemp_folder,"hourly_temperature_",stationname2,".txt", sep=""),sep=",",row.names=FALSE)
}
