
# setwd(BathyDir2014)
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(lubridate)){install.packages("lubridate")}

library(tidyverse)
library(lubridate)
library(ggplot2) 

#Read in Bathymetry Data----------------------------------------------------####
MohonkBathy<-read.csv('data/mohonkAreaBathymetry.csv', fill = TRUE)

#Read Seasonal NAO indices----------------------------------------------------####
#Read in data
NAO_daily<-read.csv("data/NAO_index_daily.csv")

#Make date column
NAO_daily$Date <- paste(NAO_daily$Year, NAO_daily$Month, NAO_daily$Day, sep="-") %>%
  ymd()

#Convert date to DOY
NAO_daily<-NAO_daily %>%
  mutate(DOY=yday(Date))

# # Read in Seasonal ENSO indices ----------------------------------------------------###
# 
# #Read in data
# ENSO_monthly<-read.csv("data/ONI_index_monthly.csv")

##Upload Mohonk NOAA National Weather Service Temp and Precip daily data####
MohonkDailyWeather.upload<-read.csv("data/MohonkPreserveWeatherData-1985-2017-NOAA-NCEI.csv", fill = TRUE)
MohonkDailyWeather.upload$Date<-as.Date(as.character(MohonkDailyWeather.upload$DATE))
MohonkDailyWeather.upload$Precip_mm<-MohonkDailyWeather.upload$PRCP_in*25.4
MohonkDailyWeather.upload$Snow_mm<-MohonkDailyWeather.upload$SNOW_in*25.4
MohonkDailyWeather.upload$SnowDepth_mm<-MohonkDailyWeather.upload$SNWD_in*25.4
MohonkDailyWeather.upload$TempMax_degC<-(MohonkDailyWeather.upload$TMAX_degF-32)*5/9
MohonkDailyWeather.upload$TempMin_degC<-(MohonkDailyWeather.upload$TMIN_degF-32)*5/9
MohonkDailyWeather.upload$TempMean_degC<-(MohonkDailyWeather.upload$TempMax_degC+MohonkDailyWeather.upload$TempMin_degC)/2

#Keep relevant columns for weather data frame
MohonkDailyWeather<-MohonkDailyWeather.upload[,c("Date","Precip_mm","Snow_mm",
                                                 "SnowDepth_mm","TempMax_degC",
                                                 "TempMin_degC","TempMean_degC")]


# Read in Seasonal ENSO indices ----------------------------------------------------###
#2020-12-10 IAO: Uploading new ENSO data 
####Source: ENSO (MEI v2) - 1979 to present, monthly
####https://psl.noaa.gov/data/climateindices/
#Read in data
ENSO_MEI_monthly<-read.csv("data/ENSO_MEI_1979-2020.csv")

#Read in data
ENSO_monthly<-read.csv("data/ONI_index_monthly.csv")


##Upload Mohonk FULL RECORD NOAA National Weather Service Temp and Precip daily data ####   
#2020-12-01 IAO downloaded wx data going back to 1930
MohonkDailyWeatherFull.upload<-read.csv("data/MohonkPreserveWeatherData-1930-2020-NOAA-NCEI.csv", fill = TRUE) 
head(MohonkDailyWeatherFull.upload)
MohonkDailyWeatherFull.upload$Date<-as.Date(as.character(MohonkDailyWeatherFull.upload$DATE))
MohonkDailyWeatherFull.upload$Precip_mm<-MohonkDailyWeatherFull.upload$PRCP*25.4 #convert to mm
MohonkDailyWeatherFull.upload$Snow_mm<-MohonkDailyWeatherFull.upload$SNOW*25.4 #convert to m
MohonkDailyWeatherFull.upload$SnowDepth_mm<-MohonkDailyWeatherFull.upload$SNWD*25.4 #convert to mm
MohonkDailyWeatherFull.upload$TempMax_degC<-(MohonkDailyWeatherFull.upload$TMAX-32)*5/9 #convert to C
MohonkDailyWeatherFull.upload$TempMin_degC<-(MohonkDailyWeatherFull.upload$TMIN-32)*5/9 #convert to C
MohonkDailyWeatherFull.upload$TempMean_degC<-(MohonkDailyWeatherFull.upload$TempMax_degC+MohonkDailyWeatherFull.upload$TempMin_degC)/2

#Keep relevant columns for weather data frame
MohonkDailyWeatherFull<-MohonkDailyWeatherFull.upload[,c("Date","Precip_mm","Snow_mm",
                                                         "SnowDepth_mm","TempMax_degC",
                                                         "TempMin_degC","TempMean_degC")]



##Upload Mohonk Ice on and Ice off data####    
MohonkIce.upload<-read.csv("data/MohonkLake-IceOnIceOff-1932-2017.csv", fill = TRUE) 
MohonkIce.upload$Year<-seq(1932,2019,by=1) #Corrected the script to include 2019


#Create new column of ice in date. Replace "No Date" with NA, format the others to dates
MohonkIce.upload$IceInDate<-as.character(MohonkIce.upload$ICEIN)
MohonkIce.upload$IceInDate[MohonkIce.upload$IceInDate=="No date"]<-NA
# MohonkIce.upload$IceInDate<-as.Date(MohonkIce.upload$IceInDate)
MohonkIce.upload <- MohonkIce.upload %>%
  mutate(IceInDate=mdy(IceInDate))

#Create new column of ice in date. Replace "No Date
MohonkIce.upload$IceOutDate<-as.character(MohonkIce.upload$ICEOUT)
MohonkIce.upload$IceOutDate[MohonkIce.upload$IceOutDate=="No date"]<-NA
MohonkIce.upload <- MohonkIce.upload %>%
  mutate(IceOutDate=mdy(IceOutDate))
# MohonkIce.upload$IceOutDate<-as.Date(MohonkIce.upload$IceOutDate)

#Create new data frame with only relevant columns
MohonkIce<-MohonkIce.upload[,c("Year","IceInDate","IceOutDate")]
MohonkIce$IceInDayofYear<-as.POSIXlt(MohonkIce$IceInDate, format = "%d%b%y")$yday+1
MohonkIce$IceOutDayofYear<-as.POSIXlt(MohonkIce$IceOutDate, format = "%d%b%y")$yday+1
MohonkIce$LengthOfIceCover_days<-as.numeric(MohonkIce$IceOutDate-MohonkIce$IceInDate)
MohonkIcePost1985<-MohonkIce[MohonkIce$Year>=1984,]
