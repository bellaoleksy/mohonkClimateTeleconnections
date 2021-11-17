#Analysis and figure generation to check methods and for supplemental material
  #*Includes generation of a profile
  #*Analysis of mixing action for open water period

# Figure S3. Comparison of daily average sensor data to manual profiles --------

##Upload 2017 data
MohonkWeeklyProfiles2017<-read.csv("data/2017_MohonkLake_Secchi_Thermistor.csv", fill = TRUE)  
MohonkWeeklyProfiles2017Metric<-MohonkWeeklyProfiles2017

#Convert to temperature in C
MohonkWeeklyProfiles2017Metric[,seq(5,18)]<-(MohonkWeeklyProfiles2017Metric[,seq(5,18)]-32)*(5/9)

#Rename column headers
names(MohonkWeeklyProfiles2017Metric)<-c("Date","Collector","DepthToBottom_m",
                                         "Comment","Temp_0m","Temp_1m","Temp_2m",
                                         "Temp_3m","Temp_4m","Temp_5m","Temp_6m",
                                         "Temp_7m","Temp_8m","Temp_9m","Temp_10m",
                                         "Temp_11m","Temp_12m","Temp_Bottom",
                                         "Secchi_m")

#Convert date variable to date
MohonkWeeklyProfiles2017Metric$Date<-as.Date(as.character(MohonkWeeklyProfiles2017Metric$Date))
str(MohonkWeeklyProfiles2017Metric)

#Convert to m from feet
MohonkWeeklyProfiles2017Metric$Secchi_m<-MohonkWeeklyProfiles2017Metric$Secchi_m*0.3048  

#Any secchi with 0 gets converted to NA
MohonkWeeklyProfiles2017Metric$Secchi_m[MohonkWeeklyProfiles2017Metric$Secchi_m==0]<-NA

#Add an extra column called "BOTTOM" that is NA to match with <2016 data
MohonkWeeklyProfiles2017Metric$BOTTOM<-NA

##Create a separate file for manual 2017 collection####
MohonkWeeklyProfiles2017metric.manual<-MohonkWeeklyProfiles2017Metric

##Upload sensor data####    
MohonkWeeklyProfilesSensor<-read.csv("data/MohonkSensor-AllData-01Apr2016to06Aug2018-ModifiedHeaders.csv", fill = TRUE)  

#Fix date/time format
MohonkWeeklyProfilesSensor$DateTime<-as.POSIXct(as.character(MohonkWeeklyProfilesSensor$DateTime))

#Remove the -100000 error code
#Loop through all the temperature readings for quality control histograms
for(j in 2:11){
  MohonkWeeklyProfilesSensor[MohonkWeeklyProfilesSensor[,j]<(-20),j]<-NA
  #hist(MohonkWeeklyProfilesSensor[,j],main=names(MohonkWeeklyProfilesSensor)[j])
}

#Remove all rows of data where Temp_9m>13C
MohonkWeeklyProfilesSensor[MohonkWeeklyProfilesSensor$Temp_9m>13|is.na(MohonkWeeklyProfilesSensor$Temp_9m),2:11]<-NA

#Downsample to daily values
#Create a date column
MohonkWeeklyProfilesSensor$Date<-as.Date(MohonkWeeklyProfilesSensor$DateTime)

#aggregate by date for each column
MohonkWeeklyProfilesSensorDaily<-aggregate(MohonkWeeklyProfilesSensor,by=list(MohonkWeeklyProfilesSensor$Date),FUN=mean,na.rm=T)

#Add some dummy columns to match up with the other data frames
MohonkWeeklyProfilesSensorDaily$Collector<-NA
MohonkWeeklyProfilesSensorDaily$DepthToBottom_m<-NA
MohonkWeeklyProfilesSensorDaily$Comment<-NA
MohonkWeeklyProfilesSensorDaily$Temp_10m<-NA
MohonkWeeklyProfilesSensorDaily$Temp_11m<-NA
MohonkWeeklyProfilesSensorDaily$Temp_12m<-NA
MohonkWeeklyProfilesSensorDaily$Temp_Bottom<-NA
MohonkWeeklyProfilesSensorDaily$Secchi_m<-NA
MohonkWeeklyProfilesSensorDaily$BOTTOM<-NA

#Output the correct order
MohonkWeeklyProfilesSensorDaily.cols<-MohonkWeeklyProfilesSensorDaily[,names(MohonkWeeklyProfiles2017Metric)]

#Merge 2017 MP data with 2017 sensor daily data with MP data superceding the sensor data
#Second is the sensor data frame for all dates except those found in MohonkWeeklyProfiles2017Metric$Date and year = 2017
MohonkWeeklyProfiles2017Metric<-rbind(MohonkWeeklyProfiles2017Metric,
                                      MohonkWeeklyProfilesSensorDaily.cols[!(MohonkWeeklyProfilesSensorDaily.cols$Date %in% MohonkWeeklyProfiles2017Metric$Date)&as.numeric(format(MohonkWeeklyProfilesSensorDaily.cols$Date,"%Y"))==2017,])


#Create new data frames for the sensor data
MohonkWeeklyProfiles2017metric.sensor<-MohonkWeeklyProfilesSensor%>%filter(year(Date)=="2017")
MohonkWeeklyProfiles2017metric.sensor.daily<-MohonkWeeklyProfilesSensorDaily.cols%>%filter(year(Date)=="2017")

#Order by date
MohonkWeeklyProfiles2017Metric<-MohonkWeeklyProfiles2017Metric[order(MohonkWeeklyProfiles2017Metric$Date),]



ProfileComparison2017<-left_join(MohonkWeeklyProfiles2017metric.manual%>%
                                   dplyr::select(Date,Temp_0m:Temp_9m)%>%
                                   pivot_longer(cols=Temp_0m:Temp_9m)%>%
                                   rename(Measurement=name,Temp_degC_manual=value),
                                 MohonkWeeklyProfiles2017metric.sensor.daily%>%
                                   filter(Date %in% MohonkWeeklyProfiles2017metric.manual$Date)%>%
                                   dplyr::select(Date,Temp_0m:Temp_9m)%>%
                                   pivot_longer(cols=Temp_0m:Temp_9m)%>%
                                   rename(Measurement=name,Temp_degC_sensor=value),
                                 by=c("Date","Measurement"))


##Plot all values compared to each other, not necessary to do individual ones####
gg.2017sensormanualcomparison<-
  ggplot(data=ProfileComparison2017,aes(x=Temp_degC_manual,y=Temp_degC_sensor))+
  geom_point(size=1.15)+
  theme_bw()+
  ylab(bquote(Sensor~temperature~(degree*C)))+
  xlab(bquote(Manual~temperature~(degree*C)))+
  theme_MS()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  geom_smooth(method = "lm", se = FALSE,col="blue")+
  geom_abline(slope=1,intercept=0,size=1.15,col="red")

# ggsave("figures/supplementary/figureS3.sensorvsmanualcomparison.jpg",
#        plot=gg.2017sensormanualcomparison,
#        width = 3.3, height = 3, units = "in",dpi = 300)

#**linear regression between the two
summary(lm(ProfileComparison2017$Temp_degC_sensor~ProfileComparison2017$Temp_degC_manual))




#Figure S4. Select a single profile --------------------------------------------
#Profile generation
#Here I picked 2000-08-10 randomly as high stratification from weekly (not interpolated dataset)
SingleProfile<-MohonkWeeklyProfilesMetric%>%
  filter(Date=="2000-08-10")%>%
  select(Temp_0m:Temp_13m) %>%
  pivot_longer(cols=starts_with("Temp"))%>%
  separate(name, c("name","depths"), sep="_") %>%
  separate(depths, c("depths","unit"), sep="m") %>%
  dplyr::select(-name, -unit) %>%
  mutate(depths=as.numeric(as.character(depths)))

gg.singleprofile<-ggplot(data=SingleProfile,aes(x=value,y=depths))+
  geom_hline(yintercept=1,size=1.15,col="light blue")+
  geom_hline(yintercept=3,size=1.15,col="light blue")+
  geom_hline(yintercept=10,size=1.15,col="dark blue")+
  geom_hline(yintercept=12,size=1.15,col="dark blue")+
  geom_point(size=3)+
  geom_line(size=1.15)+
  scale_y_continuous(trans = "reverse",lim=c(13.5,0),breaks=rev(seq(0,12,by=4)))+
  theme_bw()+
  ylab("Depth (m)")+
  xlab(bquote(Temperature~(degree*C)))+
  theme_MS()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

gg.singleprofile

#Export as a jpg
ggsave("figures/supplementary/figureS4.exampleProfile.jpg",plot=gg.singleprofile, width = 3.3, height = 3.6, units = "in",dpi = 300)




#Figure S6. Single years worth of MA -------------------------------------------
#with lines indicating start and end of stratifiication and open water period
year.select<-1985
gg.annualstabilityPlot<-ggplot(data=DailyInterpol%>%filter(year==year.select),
                               aes(x=dayofyear,y=stability_Jperm2))+
  geom_vline(xintercept=as.numeric(AnnualData%>%filter(Year==year.select)%>%
                                     select(IceOutDayofYear)),size=1.15,col="grey")+
  geom_vline(xintercept=as.numeric(AnnualData%>%filter(Year==year.select)%>%
                                     select(IceInDayofYear)),size=1.15,col="grey")+
  geom_vline(xintercept=as.numeric(AnnualData%>%filter(Year==year.select)%>%
                                     select(StartOfStratification_Day)),size=1.15,col="dark blue")+
  geom_vline(xintercept=as.numeric(AnnualData%>%filter(Year==year.select)%>%
                                     select(EndOfStratification_Day)),size=1.15,col="dark blue")+
  geom_hline(yintercept=64)+
  geom_point(size=1.15)+
  theme_bw()+
  theme_MS()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  ylab(bquote(Schmidt~stability~(J~m^-2)))+
  xlab(bquote(Day~of~year))

# ggsave("figures/supplementary/figureS6.AYearOfStability.jpg",plot=gg.annualstabilityPlot, width = 3.3, height = 2.5, units = "in",dpi = 300)




#Figure S7. Mixing Action during full open water season versus stratified period-----
    #*Plot comparison of open water vs. stratified mixing action
gg.mixingaction.comparison<-ggplot(data=AnnualData,aes(x=MixingAction_gigaJday,
                                                           y=MixingAction.OpenWater_gigaJday))+
    theme_MS()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    geom_smooth(method = "lm", se = FALSE,col="blue")+
    geom_abline(slope=1,intercept=0,size=1.15,col="red")+
    geom_point(size=1.15)+
    ylab(bquote(Open~water~MA~(GJ~day)))+
    xlab(bquote(Stratified~MA~(GJ~day)))+
    scale_x_continuous(lim=c(3.15,4.5),breaks=c(3.5,4,4.5))+
    scale_y_continuous(lim=c(3.15,4.5),breaks=c(3.5,4,4.5))

# ggsave("figures/supplementary/figureS7.MixingActioncomparison.jpg",
#        plot=gg.mixingaction.comparison,
#        width = 3.3, height = 3.3, units = "in",dpi = 300)

  #**linear regression between the two
  summary(lm(AnnualData$MixingAction.OpenWater_gigaJday~AnnualData$MixingAction_gigaJday))
  min(AnnualData$MixingAction_gigaJday,na.rm=T)

  #**Figuring out the % increase by expanding to open water season
  MA.increase<-(AnnualData$MixingAction.OpenWater_gigaJday-AnnualData$MixingAction_gigaJday)*100/AnnualData$MixingAction_gigaJday
  summary(MA.increase)
  sd(MA.increase,na.rm=T)

  
  #Declutter Global Environment
  rm(list = ls()[grep("MohonkWeekly", ls())])
  rm(list = ls()[grep("gg.", ls())])
  