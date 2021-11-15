#R script created 15May2020 by DCR####
#Analysis and figure generation to check methods and for supplemental material
  #*Includes generation of a profile####
  #*Analysis of mixing action for open water period####

#Profile generation####
#*Select a single profile####
#Here I picked 2000-08-10 randomly as high stratification from weekly (not interpolated dataset)
SingleProfile<-MohonkWeeklyProfilesMetric%>%filter(Date=="2000-08-10")%>%select(Temp_0m:Temp_Bottom)%>%pivot_longer(cols=starts_with("Temp"))%>%bind_cols(depths=depths.v)%>%sort()

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
ggsave("figures/exploratory/MTCC-ExampleProfile.jpg",plot=gg.singleprofile, width = 3.3, height = 3.6, units = "in",dpi = 300)



#Comparison of daily average sensor data to manual profiles####
ProfileComparison2017<-left_join(MohonkWeeklyProfiles2017metric.manual%>%dplyr::select(Date,Temp_0m:Temp_9m)%>%pivot_longer(cols=Temp_0m:Temp_9m)%>%rename(Measurement=name,Temp_degC_manual=value),
                                 MohonkWeeklyProfiles2017metric.sensor.daily%>%filter(Date %in% MohonkWeeklyProfiles2017metric.manual$Date)%>%dplyr::select(Date,Temp_0m:Temp_9m)%>%pivot_longer(cols=Temp_0m:Temp_9m)%>%rename(Measurement=name,Temp_degC_sensor=value),by=c("Date","Measurement"))


#*Plot all values compared to each other, not necessary to do individual ones####
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

  ggsave("figures/exploratory/MTCC-sensorvsmanualcomparison.jpg",plot=gg.2017sensormanualcomparison, width = 3.3, height = 3, units = "in",dpi = 300)
  #**linear regression between the two####
  summary(lm(ProfileComparison2017$Temp_degC_sensor~ProfileComparison2017$Temp_degC_manual))

  
  

#Mixing Action - create an open water season one in data munging then do the analysis here####
    #*Plot comparison of open water vs. stratified mixing action####
    gg.mixingaction.comparison<-ggplot(data=AnnualData,aes(x=MixingAction_gigaJday,y=MixingAction.OpenWater_gigaJday))+
    theme_bw()+
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

  ggsave("figures/exploratory/MTCC-MixingActioncomparison.jpg",plot=gg.mixingaction.comparison, width = 3.3, height = 3.3, units = "in",dpi = 300)
  #**linear regression between the two####
  summary(lm(AnnualData$MixingAction.OpenWater_gigaJday~AnnualData$MixingAction_gigaJday))
  min(AnnualData$MixingAction_gigaJday,na.rm=T)
  #**Figuring out the % increase by expanding to open water season####
  MA.increase<-(AnnualData$MixingAction.OpenWater_gigaJday-AnnualData$MixingAction_gigaJday)*100/AnnualData$MixingAction_gigaJday
  summary(MA.increase)
  sd(MA.increase,na.rm=T)
  
  #Plot a single years worth of MA with lines indicating start and end of stratifiication and open water period
  year.select<-1985
  gg.annualstabilityPlot<-ggplot(data=DailyInterpol%>%filter(year==year.select),aes(x=dayofyear,y=stability_Jperm2))+
    geom_vline(xintercept=as.numeric(AnnualData%>%filter(Year==year.select)%>%select(IceOutDayofYear)),size=1.15,col="grey")+
    geom_vline(xintercept=as.numeric(AnnualData%>%filter(Year==year.select)%>%select(IceInDayofYear)),size=1.15,col="grey")+
    geom_vline(xintercept=as.numeric(AnnualData%>%filter(Year==year.select)%>%select(StartOfStratification_Day)),size=1.15,col="dark blue")+
    geom_vline(xintercept=as.numeric(AnnualData%>%filter(Year==year.select)%>%select(EndOfStratification_Day)),size=1.15,col="dark blue")+
    geom_hline(yintercept=64)+
    geom_point(size=1.15)+
    theme_bw()+
    theme_MS()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ylab(bquote(Schmidt~stability~(J~m^-2)))+
    xlab(bquote(Day~of~year))
  
    ggsave("figures/exploratory/MTCC-AYearOfStability.jpg",plot=gg.annualstabilityPlot, width = 3.3, height = 2.5, units = "in",dpi = 300)
  
  
  
  