#R script created 20Dec2019 by DCR####
#Updated 01Feb2020 with IAO and DCR to add figures and analyses for basic trends of all variables
  #Includes key manuscript figures at the bottom

#Packages####
if(!require(reshape2)){install.packages("reshape2")}
if(!require(scales)){install.packages("scales")}
if(!require(ggthemes)){install.packages("ggthemes")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(ggridges)){install.packages("ggridges")}
if(!require(egg)){install.packages("egg")}
if(!require(gridExtra)){install.packages("gridExtra")}
if(!require(huxtable)){install.packages("huxtable")}
if(!require(officer)){install.packages("officer")}
if(!require(flextable)){install.packages("flextable")}

#Load libraries
library(reshape2)
library(scales) #for pretty_breaks()
library(ggthemes)
library(ggpubr)
library(ggridges)
library(egg)
library(gridExtra)
library(huxtable) #Pretty tables
library(officer) #exporting pretty tables to word
library(flextable) #exporting pretty tables to word

# 
# #Extract Mohonk Bathymetry proportons####
# #Sum up the volume of the lake that we cover
# MohonkBathy.volume%>%
#   filter(UpperDepth_m<=13)%>%
#   summarize(sum=sum(Volume_proportion))


####Weekly trends over the years####
####COULD MODIFY THIS FOR DIFFERENT VARIABLES; MAKE SURE TO INCLUDE CORRECTED P.VALUE
####Did it for Epi temp and BF 
####Look for weekly trends (weeks 1 to 52) over the years for the following variables:
#"thermoclineDepth_m_thresh0.1","stability_Jperm2","buoyancyfrequency_1_s2","EpiTemp_degC","HypoTemp_degC","VolumeWeightedMeanTemp_degC" 
#week.index<-1
weeklyAnalysis.DF<-data.frame(weekofyear=seq(1,52,by=1))

for(week.index in 1:52){
  y.var<-"stability_Jperm2"
  tmp.weekData<-DailyInterpol[DailyInterpol$weekofyear==week.index,c("year",y.var)]  
  #If 1985 is NA, remove it
  if(sum(!is.na(tmp.weekData[tmp.weekData$year==1985,y.var]))==0){tmp.weekData<-tmp.weekData[tmp.weekData$year>1985,]}
  tmp.weekData.agg<-setNames(aggregate(tmp.weekData[,y.var],by=list(tmp.weekData$year),FUN=mean,na.rm=T),c("year",y.var))
  #plot(tmp.weekData.agg[,y.var]~tmp.weekData.agg$year)
  #Extend data out to include missing years
  tmp.weekData.impute<-merge(tmp.weekData.agg,data.frame(year=seq(min(tmp.weekData.agg$year),max(tmp.weekData.agg$year),by=1)),by="year",all.y=T)
  
  #Impute data for 1997
  tmp.weekData.impute[tmp.weekData.impute$year==1997,y.var]<-mean(tmp.weekData.impute[tmp.weekData.impute$year==1995|tmp.weekData.impute$year==1996|tmp.weekData.impute$year==1998|tmp.weekData.impute$year==1999,y.var])
  #Impute data for 2014
  tmp.weekData.impute[tmp.weekData.impute$year==2014,y.var]<-mean(tmp.weekData.impute[tmp.weekData.impute$year==2012|tmp.weekData.impute$year==2013|tmp.weekData.impute$year==2015|tmp.weekData.impute$year==2016,y.var])
  
  
  ####*SENS SLOPE for the weekly variables of interest
  s.slope<-sens.slope(tmp.weekData.impute[,y.var])
  #Store the sen slope and the p value
  weeklyAnalysis.DF[week.index,paste("SenSlope_",y.var,sep="")]<-s.slope$estimates
  weeklyAnalysis.DF[week.index,paste("SenSlopePvalue_",y.var,sep="")]<-s.slope$p.value
}

#Need to adjust to alpha for 52 analyses
#Sidak adjusts to 0.0009859 or rounds to 0.001 for 52 comparisons
plot(weeklyAnalysis.DF[,paste("SenSlope_",y.var,sep="")]~weeklyAnalysis.DF$weekofyear)
points(weeklyAnalysis.DF[weeklyAnalysis.DF[,paste("SenSlopePvalue_",y.var,sep="")]<0.001,paste("SenSlope_",y.var,sep="")]~weeklyAnalysis.DF$weekofyear[weeklyAnalysis.DF[,paste("SenSlopePvalue_",y.var,sep="")]<0.001],col="blue",pch=19)
abline(h=0)

####EXPLORATORY FIGURE: Annual trends: Mixing action alone vs. year####
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MohonkLake-YearInLife-ExploratoryFigure-MixingAction.jpg", width = 6, height = 4, units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

#Series of y labels
ylabels<-c(expression(Surface~Temp~(degree*C)),expression(Deep~Temp~(degree*C)),expression(Mixing~Action~(GJ~day)))

#Plot the stratified season average temperature
plot(AnnualData$MixingAction_gigaJday~AnnualData$Year,xaxt='n',yaxt='n')
mtext(ylabels[3],side=2,line=2.2,cex=0.8)
axis(side=2,at=c(seq(3.3,4.5,0.3)))
#X axis labels    
mtext("Year",side=1,line=2.2,cex=0.8)
axis(1,at=seq(1985,2015,10),labels=seq(1985,2015,10))

dev.off()  
par(op)      

####EXPLORATORY FIGURE: Annual trends: Start, end, and length of stratification vs. year####
####Based on stability metrics for stratification
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StartEndLengthOfStratificationFigure.jpg", width = 3.3, height = 6.3, units = "in",res = 300)
par(mfrow=c(3,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

#Series of y labels
ylabels<-c(expression(Start~of~Strat~(day~of~year)),expression(End~of~Strat~(day~of~year)),expression(Length~of~strat~(days)))

#Plot the stratified season average temperature
plot(AnnualData$StartOfStratification_Day~AnnualData$Year,xaxt='n',yaxt='n',ylim=c(90,134))
mtext(ylabels[1],side=2,line=2.2,cex=0.8)
#Y axis label
axis(side=2,at=seq(90,130,by=10))

#Plot the stratified season average temperature
plot(AnnualData$EndOfStratification_Day~AnnualData$Year,xaxt='n',yaxt='n',ylim=c(280,314))
mtext(ylabels[2],side=2,line=2.2,cex=0.8)
#Y axis label
axis(side=2,at=seq(280,310,by=10))

#Plot the stratified season average temperature
plot(AnnualData$LengthOfStrat_days~AnnualData$Year,xaxt='n',ylim=c(160,217))
mtext(ylabels[3],side=2,line=2.2,cex=0.8)
#X axis labels    
mtext("Year",side=1,line=2.2,cex=0.8)
axis(1,at=seq(160,210,10))
axis(1,at=seq(1985,2015,10),labels=seq(1985,2015,10))

dev.off()  
par(op)

####STATS: Annual trends and breakpoint####

#*Sen slopes using DCR defined function that is drawn from zyp and trend functions####
#No longer need to impute values or use zyp.sen
#Deals with NA values
#Use MTCC.sensSlope(x=AnnualData$Year,y=AnnualData$VARIABLE) 
#create new df with variable name, sen slope, intercept, z, n, pvalue

#Melts Annual Data into long form using reshape2
AnnualData.long<-melt(AnnualData,id="Year")

#**Runs Theil Sen's slopes on each variable####
#Includes summary stats of significant slopes
AnnualData.SensSlopeSummary<-
  AnnualData.long%>%
  group_by(variable)%>%
  summarize(Sens_Slope=MTCC.sensSlope(x=Year,y=value)$coefficients["Year"],
            Sens_Intercept=MTCC.sensSlope(x=Year,y=value)$coefficients["Intercept"],
            Sens_pval=MTCC.sensSlope(x=Year,y=value)$pval,
            Sens_z_stat=MTCC.sensSlope(x=Year,y=value)$z_stat,
            Sens_n=MTCC.sensSlope(x=Year,y=value)$n)%>%
  mutate(Significance=ifelse(Sens_pval<0.05,"*","NS"))

#**write csv of the sens slopes summary####
write_csv(AnnualData.SensSlopeSummary,"figures/MTCC-TableX-SensSlopesAllVariables.csv")

# --> Export sen slope table to .docx -----------------------------------------------

AnnualData.SensSlopeSummary_hux <- 
  hux(AnnualData.SensSlopeSummary) %>% 
  add_colnames() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE) 

theme_plain(AnnualData.SensSlopeSummary_hux)
# quick_docx(AnnualData.SensSlopeSummary_hux, file = 'manuscript/senslope_output.docx')
# Commented out because I just wanted to export
# this so I could add the table to our Supplemental Materials google doc


#**Plot interannual trends of all variables####
ggplot(data=AnnualData.long,aes(x=Year,y=value))+
  geom_point()+
  facet_wrap(~variable,scale="free")

#**Plot only significant trends####
ggplot(data=left_join(AnnualData.long,AnnualData.SensSlopeSummary%>%dplyr::select(variable,Significance),by="variable")%>%filter(Significance=="*"),aes(x=Year,y=value))+
  geom_point(col="red")+
  facet_wrap(~variable,scale="free")

#**Plot only non-significant trends####
ggplot(data=left_join(AnnualData.long,AnnualData.SensSlopeSummary%>%dplyr::select(variable,Significance),by="variable")%>%filter(Significance=="NS"),aes(x=Year,y=value))+
  geom_point(col="black")+
  facet_wrap(~variable,scale="free")

#!!Could include breakpoint analysis using pettitt.test below####
#Should do it for all variables using smilar code to the sens slope implementation above
#pettitt.test(AnnualData[,"Temp.Hypo.peak"])



#STATS: Winter temperature analysis --------------------------------------------

DailyInterpol.winter<-DailyInterpol[DailyInterpol$dayofyear>=355|DailyInterpol$dayofyear<=79,]
#find the winter's first year
DailyInterpol.winter$winter.startyear<-NA
DailyInterpol.winter$winter.startyear[DailyInterpol.winter$dayofyear>=355]<-DailyInterpol.winter$year[DailyInterpol.winter$dayofyear>=355]
DailyInterpol.winter$winter.startyear[DailyInterpol.winter$dayofyear<=79]<-DailyInterpol.winter$year[DailyInterpol.winter$dayofyear<=79]-1

#find the winter's first year
DailyInterpol.winter$winter.endyear<-NA
DailyInterpol.winter$winter.endyear[DailyInterpol.winter$dayofyear>=355]<-DailyInterpol.winter$year[DailyInterpol.winter$dayofyear>=355]+1
DailyInterpol.winter$winter.endyear[DailyInterpol.winter$dayofyear<=79]<-DailyInterpol.winter$year[DailyInterpol.winter$dayofyear<=79]

#Create teh winter range
DailyInterpol.winter$winter.range<-paste(DailyInterpol.winter$winter.startyear," to ",DailyInterpol.winter$winter.endyear,sep="")

#Find the winter averages
AnnualData.winter<-aggregate(DailyInterpol.winter$Temp_1m,by=list(DailyInterpol.winter$winter.range),FUN=mean)
names(AnnualData.winter)<-c("winter.range","Temp_1m")
plot(AnnualData.winter$Temp_1m)

#Use 3 methods to determine ice on/off and length
#Bruesewitz, Pierson, visual
DailyInterpol$IceCover.Pierson0.1<-0  
DailyInterpol$IceCover.Pierson0.4<-0 
DailyInterpol$IceCover.Pierson0.1[DailyInterpol$Temp_10m-DailyInterpol$Temp_0m>0.1]<-1
DailyInterpol$IceCover.Pierson0.4[DailyInterpol$Temp_10m-DailyInterpol$Temp_0m>0.4]<-1

#Create ice cover variable from the visual observations from Mohonk DSRC
DailyInterpol$IceCover.Visual<-0  
for(iii in 1:length(MohonkIcePost1985$year)){
  DailyInterpol$IceCover.Visual[DailyInterpol$Date>=MohonkIcePost1985$IceInDate[iii]&DailyInterpol$Date<=MohonkIcePost1985$IceOutDate[iii]]<-1  
}

plot(DailyInterpol$IceCover.Pierson0.1~DailyInterpol$Date,col="white")
lines(DailyInterpol$IceCover.Pierson0.1~DailyInterpol$Date,col="green")
lines(DailyInterpol$IceCover.Pierson0.4-0.2~DailyInterpol$Date,col="blue")
lines(DailyInterpol$IceCover.Visual-0.4~DailyInterpol$Date,col="red")

#Check here the change in Ice length, ice in, ice out days
plot(MohonkIcePost1985$LengthOfIceCover_days~MohonkIcePost1985$Year)
temp<-MohonkIcePost1985$IceInDayofYear
temp[MohonkIcePost1985$IceInDayofYear<200]<-temp[MohonkIcePost1985$IceInDayofYear<200]+365




#MANUSCRIPT FIGURES####


#*EXPLORATORY FIGURE: Composite Thermocline and Schmidt stability figure in BASE R####
#Get the quantiles including min, 25th, median, 75th, max
tmp.composite<-aggregate(DailyInterpol$stability_Jperm2,
                         by=list(DailyInterpol$dayofyear),
                         FUN=quantile,na.rm=T)

tmp2.composite<-aggregate(DailyInterpol$stability_Jperm2,
                          by = list(DailyInterpol$dayofyear),
                          FUN = function(x) quantile(x, probs = c(0.05,0.95),na.rm=T))

Stability.composite<-data.frame(dayofyear=tmp.composite$Group.1,
                                Min.stability_Jperm2=tmp.composite[["x"]][,1],
                                Fifth.stability_Jperm2=tmp2.composite[["x"]][,1],
                                TwentyFifth.stability_Jperm2=tmp.composite[["x"]][,2],
                                Median.stability_Jperm2=tmp.composite[["x"]][,3],
                                SeventyFifth.stability_Jperm2=tmp.composite[["x"]][,4],
                                NinetyFifth.stability_Jperm2=tmp2.composite[["x"]][,2],
                                Max.stability_Jperm2=tmp.composite[["x"]][,5])  

#Do the same for thermocline depth
tmp.composite<-aggregate(DailyInterpol$thermoclineDepth_m_thresh0.1,
                         by=list(DailyInterpol$dayofyear),
                         FUN=quantile,
                         na.rm=T)

tmp2.composite<-aggregate(DailyInterpol$thermoclineDepth_m_thresh0.1,
                          by = list(DailyInterpol$dayofyear),
                          FUN = function(x) quantile(x, probs = c(0.05,0.95),
                                                     na.rm=T))

ThermoclineDepth.composite<-data.frame(dayofyear=tmp.composite$Group.1,
                                       Min.thermoclineDepth_m_thresh0.1=tmp.composite[["x"]][,1],
                                       Fifth.thermoclineDepth_m_thresh0.1=tmp2.composite[["x"]][,1],
                                       TwentyFifth.thermoclineDepth_m_thresh0.1=tmp.composite[["x"]][,2],
                                       Median.thermoclineDepth_m_thresh0.1=tmp.composite[["x"]][,3],
                                       SeventyFifth.thermoclineDepth_m_thresh0.1=tmp.composite[["x"]][,4],
                                       NinetyFifth.thermoclineDepth_m_thresh0.1=tmp2.composite[["x"]][,2],
                                       Max.thermoclineDepth_m_thresh0.1=tmp.composite[["x"]][,5])  

#Cutoff at these days, before 75, the median is all 0.5; after 321, there is a linear increase to 1 by about 0.4 per day
#Day 321 is about the maximum median thermocline depth - probably representing mixing of the top 
lo<-75; hi<-321

#FIGURE: Composite thermocline depth and stability figure vs. day of year summarizing over all years#
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StabilityCompositeFigureOLD.jpg", width = 3.3, height = 4.2, units = "in",res = 300)
par(mfrow=c(2,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

#Plot the composite thermocline depth figure with 25th, median, and 75th percentiles
plot(ThermoclineDepth.composite$Median.thermoclineDepth_m_thresh0.1~ThermoclineDepth.composite$dayofyear,
     col="white",
     ylim=c(max(ThermoclineDepth.composite$NinetyFifth.thermoclineDepth_m_thresh0.1,na.rm=T),0),
     ylab="",
     xlab="",
     yaxt='n',
     xaxt='n')

#Plot vertical dashed lines for spring/summer/fall cutoffs
abline(v=c(80,172,265),lty=2,col="dark grey")

#Cutoff at these days, before 75, the median is all 0.5; after 321, there is a linear increase to 1 by about 0.4 per day
#Day 321 is about the maximum median thermocline depth - probably representing mixing of the top 
lo<-75; hi<-321

#Draw in sky blue from 25th to 75th percentile
polygon(c(ThermoclineDepth.composite$dayofyear[lo:hi],
          rev(ThermoclineDepth.composite$dayofyear[lo:hi]),
          ThermoclineDepth.composite$dayofyear[lo]),
        c(ThermoclineDepth.composite$Fifth.thermoclineDepth_m_thresh0.1[lo:hi],
          rev(ThermoclineDepth.composite$NinetyFifth.thermoclineDepth_m_thresh0.1[lo:hi]),
          ThermoclineDepth.composite$Fifth.thermoclineDepth_m_thresh0.1[lo]),
        col = "lightskyblue1", border = NA)

#Draw in sky blue from 25th to 75th percentile
polygon(c(ThermoclineDepth.composite$dayofyear[lo:hi],
          rev(ThermoclineDepth.composite$dayofyear[lo:hi]),
          ThermoclineDepth.composite$dayofyear[lo]),
        c(ThermoclineDepth.composite$TwentyFifth.thermoclineDepth_m_thresh0.1[lo:hi],
          rev(ThermoclineDepth.composite$SeventyFifth.thermoclineDepth_m_thresh0.1[lo:hi]),
          ThermoclineDepth.composite$TwentyFifth.thermoclineDepth_m_thresh0.1[lo]),
        col = "deepskyblue3", border = NA)

lines(ThermoclineDepth.composite$Median.thermoclineDepth_m_thresh0.1[lo:hi]~ThermoclineDepth.composite$dayofyear[lo:hi],lwd=2)
lines(ThermoclineDepth.composite$SeventyFifth.thermoclineDepth_m_thresh0.1[lo:hi]~ThermoclineDepth.composite$dayofyear[lo:hi],col="blue")
lines(ThermoclineDepth.composite$TwentyFifth.thermoclineDepth_m_thresh0.1[lo:hi]~ThermoclineDepth.composite$dayofyear[lo:hi],col="blue")
lines(ThermoclineDepth.composite$Fifth.thermoclineDepth_m_thresh0.1[lo:hi]~ThermoclineDepth.composite$dayofyear[lo:hi],col="blue")
lines(ThermoclineDepth.composite$NinetyFifth.thermoclineDepth_m_thresh0.1[lo:hi]~ThermoclineDepth.composite$dayofyear[lo:hi],col="blue")

#Y axis label
mtext(side=2,
      line=2,
      expression(Thermocline~Depth~(m)),
      cex=0.8)



#Y axis
axis(side=2,
     at=c(0,6,12),
     cex.axis=0.8,
     mgp=c(3, .5, 0))
#X axis label
#mtext(side=1,line=2,expression(Day~of~the~year),cex=0.8)
#X axis
#axis(side=1,at=c(0,100,200,300),labels=c(0,100,200,300),cex.axis=0.8,mgp=c(3, .5, 0))

#Plot the composite Schmidt stability with 25th, median, and 75th percentiles
plot(Stability.composite$Median.stability_Jperm2~Stability.composite$dayofyear,
     col="white",
     ylim=c(0,max(Stability.composite$NinetyFifth.stability_Jperm2,na.rm=T)),
     ylab="",
     xlab="",
     yaxt='n'
     ,xaxt='n')
lo<-5; hi<-360

#Plot vertical dashed lines for spring/summer/fall cutoffs
abline(v=c(80,172,265),lty=2,col="dark grey")

#Draw in sky blue from 25th to 75th percentile
polygon(c(Stability.composite$dayofyear[lo:hi],
          rev(Stability.composite$dayofyear[lo:hi]),
          Stability.composite$dayofyear[lo]),
        c(Stability.composite$Fifth.stability_Jperm2[lo:hi],
          rev(Stability.composite$NinetyFifth.stability_Jperm2[lo:hi]),
          Stability.composite$Fifth.stability_Jperm2[lo]),
        col = "lightskyblue1", border = NA)

#Draw in sky blue from 25th to 75th percentile
polygon(c(Stability.composite$dayofyear[lo:hi],
          rev(Stability.composite$dayofyear[lo:hi]),
          Stability.composite$dayofyear[lo]),
        c(Stability.composite$TwentyFifth.stability_Jperm2[lo:hi],
          rev(Stability.composite$SeventyFifth.stability_Jperm2[lo:hi]),
          Stability.composite$TwentyFifth.stability_Jperm2[lo]),
        col = "deepskyblue3", border = NA)

lines(Stability.composite$Median.stability_Jperm2~Stability.composite$dayofyear,lwd=2)
lines(Stability.composite$SeventyFifth.stability_Jperm2~Stability.composite$dayofyear,col="blue")
lines(Stability.composite$TwentyFifth.stability_Jperm2~Stability.composite$dayofyear,col="blue")
lines(Stability.composite$Fifth.stability_Jperm2~Stability.composite$dayofyear,col="blue")
lines(Stability.composite$NinetyFifth.stability_Jperm2~Stability.composite$dayofyear,col="blue")
#Y axis label
mtext(side=2,line=2,expression(Schmidt~Stability~(J~m^-2)),cex=0.8)
#Y axis
axis(side=2,at=c(0,250,500),cex.axis=0.8,mgp=c(3, .5, 0))
#X axis label
mtext(side=1,line=2,expression(Day~of~the~year),cex=0.8)
#X axis
axis(side=1,at=c(0,100,200,300),labels=c(0,100,200,300),cex.axis=0.8,mgp=c(3, .5, 0))
#horizontal abline of 11 for stratification cutoff
abline(h=Stability.cutoff,lty=2,col="red")


dev.off()
par(op)


#theme_MS()####
#!!DCR: I took out base_family="Arial"from theme_base because it does not exist in windows
#Also, changed all font sizes to 10
theme_MS <- function () { 
  theme_base(base_size=10) %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, size=1.0),
      plot.title=element_text(face="plain",hjust=0.5),
      plot.subtitle = element_text(color="dimgrey", hjust=0, size=10),
      panel.grid.minor=element_blank(), panel.grid.major=element_line(color="lightgrey"),
      strip.background = element_blank(),
      strip.text.y = element_text(size=10, angle=270),
      strip.text.x = element_text(size=10)
    )
}

#*MS FIGURE: Composite plot in ggplot####
#Cutoff at these days, before 75, the median is all 0.5; after 321, there is a linear increase to 1 by about 0.4 per day
#Day 321 is about the maximum median thermocline depth - probably representing mixing of the top 
lo<-75; hi<-321
#**Plot composite thermocline depth####
gg.thermocline.composite<-ggplot()+
  geom_line(data=ThermoclineDepth.composite%>%
               slice(lo:hi),aes(y=Median.thermoclineDepth_m_thresh0.1,x=dayofyear),
             col="white")+
  scale_y_reverse(limits=c(max(ThermoclineDepth.composite$NinetyFifth.thermoclineDepth_m_thresh0.1,na.rm=T),0),breaks=c(0,6,12))+
  scale_x_continuous(limits=c(0,365))+
  geom_vline(xintercept=c(80,172,265),lty=2,col="darkgrey")+
  geom_ribbon(data=ThermoclineDepth.composite%>%slice(lo:hi),
              aes(x=dayofyear,ymin=Fifth.thermoclineDepth_m_thresh0.1,ymax=NinetyFifth.thermoclineDepth_m_thresh0.1),
              fill="lightskyblue1")+
  geom_ribbon(data=ThermoclineDepth.composite%>%
                slice(lo:hi),
              aes(x=dayofyear,ymin=TwentyFifth.thermoclineDepth_m_thresh0.1,
                  ymax=SeventyFifth.thermoclineDepth_m_thresh0.1),fill="deepskyblue3")+
  geom_line(data=ThermoclineDepth.composite%>%
            slice(lo:hi),
            aes(x=dayofyear,y=SeventyFifth.thermoclineDepth_m_thresh0.1),lwd=0.4,col="blue")+
  geom_line(data=ThermoclineDepth.composite%>%
              slice(lo:hi),aes(x=dayofyear,
                               y=TwentyFifth.thermoclineDepth_m_thresh0.1),lwd=0.4,col="blue")+
  geom_line(data=ThermoclineDepth.composite%>%
              slice(lo:hi),aes(x=dayofyear,
                               y=Fifth.thermoclineDepth_m_thresh0.1),lwd=0.4,col="blue")+
  geom_line(data=ThermoclineDepth.composite%>%
              slice(lo:hi),aes(x=dayofyear,
                               y=NinetyFifth.thermoclineDepth_m_thresh0.1),lwd=0.4,col="blue")+
  geom_line(data=ThermoclineDepth.composite%>%
              slice(lo:hi),aes(x=dayofyear,
                               y=Median.thermoclineDepth_m_thresh0.1),lwd=0.7,col="black")+
  ylab(expression(Thermocline~Depth~(m)))+
  theme_MS()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

#**Plot composite stability###
#extend range
lo<-5; hi<-360

gg.stability.composite<-ggplot()+
  geom_line(data=Stability.composite%>%
               slice(lo:hi),aes(y=Median.stability_Jperm2,x=dayofyear),col="white")+
  scale_y_continuous(limits=c(0,max(Stability.composite$NinetyFifth.stability_Jperm2,na.rm=T)),breaks=c(0,250,500))+
  scale_x_continuous(limits=c(0,365))+
  geom_vline(xintercept=c(80,172,265),lty=2,col="darkgrey")+
  geom_ribbon(data=Stability.composite%>%slice(lo:hi),aes(x=dayofyear,ymin=Fifth.stability_Jperm2,ymax=NinetyFifth.stability_Jperm2),fill="lightskyblue1")+
  geom_ribbon(data=Stability.composite%>%slice(lo:hi),aes(x=dayofyear,ymin=TwentyFifth.stability_Jperm2,ymax=SeventyFifth.stability_Jperm2),fill="deepskyblue3")+
  geom_line(data=Stability.composite%>%slice(lo:hi),aes(x=dayofyear,y=SeventyFifth.stability_Jperm2),lwd=0.4,col="blue")+
  geom_line(data=Stability.composite%>%slice(lo:hi),aes(x=dayofyear,y=TwentyFifth.stability_Jperm2),lwd=0.4,col="blue")+
  geom_line(data=Stability.composite%>%slice(lo:hi),aes(x=dayofyear,y=Fifth.stability_Jperm2),lwd=0.4,col="blue")+
  geom_line(data=Stability.composite%>%slice(lo:hi),aes(x=dayofyear,y=NinetyFifth.stability_Jperm2),lwd=0.4,col="blue")+
  geom_line(data=Stability.composite%>%slice(lo:hi),aes(x=dayofyear,y=Median.stability_Jperm2),lwd=0.7,col="black")+
  ylab(expression(Schmidt~stability~(J~m^-2)~" "))+
  xlab(expression(Day~of~the~year))+
  geom_hline(yintercept=Stability.cutoff,lty=2,col="red")+
  theme_MS()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

#*Modify the right panel with panel letters 
panelLetter.reverse <- data.frame(
  xpos = c(-Inf),
  ypos =  c(-Inf),
  hjustvar = c(-0.5) ,
  vjustvar = c(1.5))

panelLetter.normal <- data.frame(
  xpos = c(-Inf),
  ypos =  c(Inf),
  hjustvar = c(-0.5) ,
  vjustvar = c(1.5))

#**Arrange into a two panel plot####
#***arrange into a multi-panel plot####
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
gg.2panel.composite<-ggarrange(gg.thermocline.composite+
                                 geom_text(data=panelLetter.reverse,
                                           aes(x=xpos,
                                               y=ypos,
                                               hjust=hjustvar,
                                               vjust=vjustvar,
                                               label="a",
                                               fontface="bold"))+
                          theme(axis.title.x=element_blank(),
                                axis.text.x=element_blank(),
                                axis.ticks.x=element_blank(),
                                plot.margin=unit(c(2,0.5,0,1), "lines"),
                                axis.ticks.length.x = unit(0, "pt"))+
                          labs(x=NULL, title=NULL),
                          gg.stability.composite+
                            geom_text(data=panelLetter.normal,
                                       aes(x=xpos,
                                           y=ypos,
                                           hjust=hjustvar,
                                           vjust=vjustvar,
                                           label="b",
                                           fontface="bold"))+
                          theme(plot.margin=unit(c(0,0.5,2,1), "lines"),
                                axis.title.y = element_text(margin = margin(l = 20))),
                          nrow=2)
print(gg.2panel.composite)  

#Export as a jpg
ggsave("figures/fig1.StabilityCompositeFigure.jpg",plot=gg.2panel.composite, width = 3.3, height = 4.2, units = "in",dpi = 300)




####*EXPLORATORY FIGURE: Annual trends: Summer Epi, Summer Hypo, Total Stratification vs. year####
####This is the average June 21 to Sep 21 for 1,2,3m for surface and 10,11,12 meters for deep
####Perhaps this should be average temperature during stratified period
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-FigX-SummerEpiHypoTotalStratificationFigure-OLD.jpg",
     width = 3.3, height = 6.3,
     units = "in",res = 300)
par(mfrow=c(3,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

#Series of y labels
ylabels<-c(expression(Surface~Temp~(degree*C)),
           expression(Deep~Temp~(degree*C)),
           expression(Mixing~Action~(GJ~day)))

#Plot the summer average temperature
plot(AnnualData$SurfaceWaterTemp_Summer_degC~AnnualData$Year,
     xaxt='n',yaxt='n',ylim=c(21,24.5))

mtext(ylabels[1],side=2,line=2.2,cex=0.8)

#Y axis label
axis(side=2,at=c(21,22,23,24))

#Plot the stratified season average temperature
plot(AnnualData$DeepWaterTemp_Summer_degC~AnnualData$Year,
     xaxt='n',yaxt='n')

mtext(ylabels[2],side=2,line=2.2,cex=0.8)

#Y axis label
axis(side=2,at=c(6,7,8))

#Plot the stratified season average temperature
plot(AnnualData$MixingAction_gigaJday~AnnualData$Year,
     xaxt='n',yaxt='n')

mtext(ylabels[3],side=2,line=2.2,cex=0.8)

axis(side=2,at=c(seq(3.3,4.5,0.3)))

#X axis labels    
mtext("Year",side=1,line=2.2,cex=0.8)

axis(1,at=seq(1985,2015,10),labels=seq(1985,2015,10))

dev.off()  
par(op)

####*Constructing Water & air temperature trends####
AnnualSurfaceWater<-AnnualData %>%
  dplyr::select(Year, SurfaceWaterTemp_Summer_degC, SurfaceWaterTemp_Spring_degC,
         SurfaceWaterTemp_SpringPostIce_degC) %>%
  rename(Summer=SurfaceWaterTemp_Summer_degC,
         Spring=SurfaceWaterTemp_Spring_degC,
         SpringPostIce=SurfaceWaterTemp_SpringPostIce_degC) %>%
  melt(id="Year") %>%
  rename(Season=variable,
         Temp=value) %>%
  mutate(Variable="surface",
         Water="water")
         # WaterSeason=Water+Season)
  
AnnualDeepWater<-AnnualData %>%
  dplyr::select(Year, DeepWaterTemp_Summer_degC, DeepWaterTemp_Spring_degC,
         DeepWaterTemp_SpringPostIce_degC)%>%
  rename(Summer=DeepWaterTemp_Summer_degC,
         Spring=DeepWaterTemp_Spring_degC,
         SpringPostIce=DeepWaterTemp_SpringPostIce_degC)%>%
  melt(id="Year")%>%
  rename(Season=variable,
         Temp=value) %>%
  mutate(Water="water",
         Variable="deep")

AnnualDeepWater_noSpringPostIce<-AnnualData %>%
  dplyr::select(Year, DeepWaterTemp_Summer_degC, DeepWaterTemp_Spring_degC,
         DeepWaterTemp_SpringPostIce_degC)%>%
  rename(Summer=DeepWaterTemp_Summer_degC,
         Spring=DeepWaterTemp_Spring_degC,
         SpringPostIce=DeepWaterTemp_SpringPostIce_degC)
AnnualDeepWater_noSpringPostIce$SpringPostIce<-NA
AnnualDeepWater_noSpringPostIce<-AnnualDeepWater_noSpringPostIce%>%
  melt(id="Year")%>%
  rename(Season=variable,
         Temp=value) %>%
  mutate(Water="water",
         Variable="deep")

AnnualAir<-AnnualData %>%
  dplyr::select(Year, AirTemp_Summer_degC, AirTemp_Spring_degC,
         AirTemp_SpringPostIce_degC) %>%
  rename(Summer=AirTemp_Summer_degC,
         Spring=AirTemp_Spring_degC,
         SpringPostIce=AirTemp_SpringPostIce_degC) %>%
  melt(id="Year")%>%
  rename(Season=variable,
         Temp=value) %>%
  mutate(Variable="air",
         Water="air")

AnnualAir_noSpring<-AnnualData %>%
  dplyr::select(Year, AirTemp_Summer_degC, AirTemp_Spring_degC,
         AirTemp_SpringPostIce_degC) %>%
  rename(Summer=AirTemp_Summer_degC,
         Spring=AirTemp_Spring_degC,
         SpringPostIce=AirTemp_SpringPostIce_degC) 
AnnualAir_noSpring$Spring<-NA
AnnualAir_noSpring<-AnnualAir_noSpring%>%
  melt(id="Year")%>%
  rename(Season=variable,
         Temp=value) %>%
  mutate(Variable="air",
         Water="air")

#Dataframe with all data
AnnualTemps<-bind_rows(AnnualSurfaceWater,AnnualDeepWater,AnnualAir) %>%
  mutate(WaterSeason = paste(Variable,Season,sep=""))
AnnualTemps$Variable <- as.factor(as.character(AnnualTemps$Variable))
AnnualTemps$Water <- as.factor(as.character(AnnualTemps$Water))

AnnualTemps$Season <- factor(AnnualTemps$Season, levels=c('SpringPostIce','Spring','Summer'))
AnnualTemps$Season <- factor(AnnualTemps$Season, labels=c('Spring Post-Ice','Spring','Summer'))
AnnualTemps$Variable <- factor(AnnualTemps$Variable, levels=c('air','surface','deep'))
AnnualTemps$Variable <- factor(AnnualTemps$Variable, labels=c('Air','Surface water','Deep water'))
AnnualTemps$Water <- factor(AnnualTemps$Water, labels=c('Air Temperature','Water Temperature'))
AnnualTemps$WaterSeason <- factor(AnnualTemps$WaterSeason,
                                  levels=c('airSpringPostIce','airSpring', 'airSummer',
                                           'surfaceSpringPostIce','surfaceSpring','surfaceSummer',
                                           'deepSpringPostIce','deepSpring','deepSummer'))
AnnualTemps$WaterSeason <- factor(AnnualTemps$WaterSeason,
                                  labels=c('Spring Post-Ice (air)','Spring (air)','Summer (air)',
                                           'Spring Post-Ice (surface)','Spring (surface)','Summer (surface)',
                                           'Spring Post-Ice (deep)','Spring (deep)','Summer (deep)'))

#Dataframe without Spring Air and SpringPostIce Deep values
AnnualTemps_noSpringAir<-bind_rows(AnnualSurfaceWater,AnnualDeepWater_noSpringPostIce,AnnualAir_noSpring) %>%
  mutate(WaterSeason = paste(Variable,Season,sep=""))
AnnualTemps_noSpringAir$Variable <- as.factor(as.character(AnnualTemps_noSpringAir$Variable))
AnnualTemps_noSpringAir$Water <- as.factor(as.character(AnnualTemps_noSpringAir$Water))

AnnualTemps_noSpringAir$Season <- factor(AnnualTemps_noSpringAir$Season, levels=c('SpringPostIce','Spring','Summer'))
AnnualTemps_noSpringAir$Season <- factor(AnnualTemps_noSpringAir$Season, labels=c('Spring Post-Ice','Spring','Summer'))
AnnualTemps_noSpringAir$Variable <- factor(AnnualTemps_noSpringAir$Variable, levels=c('air','surface','deep'))
AnnualTemps_noSpringAir$Variable <- factor(AnnualTemps_noSpringAir$Variable, labels=c('Air','Surface water','Deep water'))
AnnualTemps_noSpringAir$Water <- factor(AnnualTemps_noSpringAir$Water, labels=c('Air Temperature','Water Temperature'))
AnnualTemps_noSpringAir$WaterSeason <- factor(AnnualTemps_noSpringAir$WaterSeason,
                                  levels=c('airSpringPostIce','airSpring', 'airSummer',
                                           'surfaceSpringPostIce','surfaceSpring','surfaceSummer',
                                           'deepSpringPostIce','deepSpring','deepSummer'))
AnnualTemps_noSpringAir$WaterSeason <- factor(AnnualTemps_noSpringAir$WaterSeason,
                                  labels=c('Spring Post-Ice (air)','Spring (air)','Summer (air)',
                                           'Spring Post-Ice (surface)','Spring (surface)','Summer (surface)',
                                           'Spring Post-Ice (deep)','Spring (deep)','Summer (deep)'))


TempColors<- c( "#a6611a", "#80cdc1","#018571") #treatment colors


# AnnualTemps %>%
#   ggplot(aes(x=Year, y=Temp, color=Variable))+
#   geom_point(aes(colour=Variable), size=2) + 
#   geom_point(shape = 1,size = 2,colour = "black")+
#   facet_grid(vars(Water), vars(Season), scales="free")+
#   scale_color_manual(values=TempColors,
#                     name="Variable")+
#   ylab("Temperature (°C)")+
#   # coord_cartesian(ylim=c(0,25), xlim=c(1980,2020))+
#   # scale_y_continuous(breaks=seq(0, 25, 5))+
#   # scale_x_continuous(breaks=seq(1985, 2020, 10))+
#   theme_MS()+
#   theme(legend.position="none",
#         panel.spacing=grid::unit(0,"lines"))## squash panels together

# panel2x2<- 
#   ggplot(data=AnnualTemps_noSpringAir, aes(x=Year, y=Temp))+
#   geom_point(data=AnnualTemps,aes(colour=Variable, fill=Variable),
#              shape=21, colour="black",size=2) + 
#   geom_smooth(method="lm", se=FALSE, size=0.5, color="grey50")+
#   facet_grid(vars(Water), vars(Season), scales="free")+
#   scale_fill_manual(values=TempColors,
#                      name="Variable")+
#   ylab("Temperature (°C)")+
#   # coord_cartesian(ylim=c(0,25), xlim=c(1980,2020))+
#   # scale_y_continuous(breaks=seq(0, 25, 5))+
#   # scale_x_continuous(breaks=seq(1985, 2020, 10))+
#   theme_MS()+
#   theme(legend.position="none",
#         panel.spacing=grid::unit(0,"lines"))## squash panels together
# panel2x2

#*MS FIGURE: Air surface and deep temperature, with spring post-ice, spring, and summer time periods, 3x3####
#Plots 3x3 with a common axis for Air, Surface water, Deep water


#A function for make different breaks for each panel. 
breaks_fun <- function(y) {
  if (max(y) < 10) {
    seq(4, 8, 1)
  } else {
    seq(5, 20, 5)
  }
}

panel3x3<-
  ggplot(data=AnnualTemps_noSpringAir, aes(x=Year, y=Temp))+
  geom_point(data=AnnualTemps,aes(colour=Variable, fill=Variable),
             shape=21, colour="black",size=2) + 
  geom_smooth(method="lm", se=FALSE, size=0.5, color="grey50")+
  facet_grid(vars(Variable), vars(Season), scales="free_y", margins=FALSE)+
  scale_fill_manual(values=TempColors,
                     name="Variable")+
  ylab(bquote(Temperature~(degree*C)))+
  # coord_cartesian(ylim=c(0,25), xlim=c(1980,2020))+
  scale_y_continuous(breaks = breaks_fun)+
  scale_x_continuous(limits=c(1984,2018),breaks=seq(1985, 2020, 10))+
  theme_MS()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # axis.line = element_line(colour = "black"),
        legend.position="none",
        panel.spacing=grid::unit(0,"lines"))## squash panels together
panel3x3<-tag_facet(panel3x3, open=NULL, close=NULL)
#tag_facet() removes the strip.text and background, so add it back here.
panel3x3 <- panel3x3 +
  theme(strip.text = element_text())
print(panel3x3)


##NOT elegant, but how I added sen slope labels
  WaterSeason<-c('Spring Post-Ice (air)',
                 'Spring (air)',
                 'Summer (air)',
                 'Spring Post-Ice (surface)',
                 'Spring (surface)',
                 'Summer (surface)',
                 'Spring Post-Ice (deep)',
                 'Spring (deep)',
                 'Summer (deep)')
  labelSen<-c(0.173,
              NA,
              0.043,
              0.055,
              0.039,
              0.048,
              NA,
              -0.032,
              -0.049)
senLabels<-data.frame(WaterSeason,labelSen)

##Labels to graph
  panel3x3+
    geom_text(data = senLabels %>% #First three rows
                filter(WaterSeason=="Spring Post-Ice (air)"),
                # filter(Water == "Air Temperature" &
                #       Season == "Spring Post-Ice"),
               x=1995, y = 18,
              label = c('0.173°C/year',NA,'0.04°C/year',NA,NA,NA,NA,NA,NA),
              size = 3.5)+
    geom_text(data = senLabels %>% #Middle three rows
                filter(WaterSeason=="Spring Post-Ice (air)"),
              # filter(Water == "Air Temperature" &
              #       Season == "Spring Post-Ice"),
              x=1995, y = 18,
              label = c(NA,NA,NA,'0.06°C/year','0.04°C/year','0.05°C/year',NA,NA,NA),
              size = 3.5)+
    geom_text(data = senLabels %>% #Last three rows
                filter(WaterSeason=="Spring Post-Ice (air)"),
              # filter(Water == "Air Temperature" &
              #       Season == "Spring Post-Ice"),
              x=1995, y = 4,
              label = c(NA,NA,NA,NA,NA,NA,NA,'-0.03°C/year',"-0.05°C/year"),
              size = 3.5)
    # geom_text(data = senLabels %>%
    #             filter(WaterSeason=="Spring Post-Ice (deep)"),
    #           # filter(Water == "Air Temperature" &
    #           #       Season == "Spring Post-Ice"),
    #           x=2013, y = c(5,5,4),
    #           label = c(NA,NA,NA,NA,NA,NA,NA,'-0.032',"-0.049"),
    #           size = 3.5)
  #   geom_text(data = senLabels %>%
  #               filter(WaterSeason=="Spring (air)"),
  #             # filter(Water == "Air Temperature" &
  #             #       Season == "Spring Post-Ice"),
  #             x=2013, y = c(5,5,4),
  #             label = c(NA,'0.039',"-0.032"),
  #             size = 3.5) +
  # geom_text(data = senLabels %>%
  #             filter(WaterSeason=="Summer (air)"),
  #           # filter(Water == "Air Temperature" &
  #           #       Season == "Spring Post-Ice"),
  #           x=2013, y = c(5,5,4),
  #           label = c('0.043','0.048',"-0.049"),
  #           size = 3.5) 

  
# jpeg("/Users/solomonlab/Desktop/MTCC-FigX-AirAndWaterTempTrends.jpg", width = 6.6, height = 5, units = "in",res = 600)
# ggsave("/Users/solomonlab/Desktop/MTCC-FigX-AirAndWaterTempTrends.jpg", width=6.6, height=5,units="in")
ggsave("figures/MTCC-FigX-AirAndWaterTempTrends_shorter.jpg", width=6.6, height=5,units="in")

panel3x3
dev.off()

#Plots 3x3 with a common axis for Air, Surface water, Deep water## But separate y-scales for each panel.
## Unfortuantely can't figure out a way to make the y-scales free with
## facet_grid.  With facet_wrap(), can't figure out how to make the x-labels
## appear on top and the y-labels to appear on the side. 
# panel3x3_alt<-
#   ggplot(data=AnnualTemps_noSpringAir, aes(x=Year, y=Temp))+
#   geom_point(data=AnnualTemps,aes(colour=Variable, fill=Variable),
#              shape=21, colour="black",size=2) + 
#   geom_smooth(method="lm", se=FALSE, size=0.5, color="grey50")+
#   # facet_grid(Variable~Season, scales="free_y", margins=FALSE)+
#   facet_wrap(.~WaterSeason, scales="free_y",
#              strip.position="right")+
#   scale_fill_manual(values=TempColors,
#                     name="Variable")+
#   ylab("Temperature (°C)")+
#   # coord_cartesian(ylim=c(0,25), xlim=c(1980,2020))+
#   # scale_y_continuous(breaks=seq(0, 25, 5))+
#   scale_x_continuous(breaks=seq(1985, 2020, 10))+
#   theme_MS()+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black"),
#         legend.position="none",
#         panel.spacing=grid::unit(0,"lines"),
#         strip.text.x = element_blank())## squash panels together
# panel3x3_alt

####*EXPLORATORY FIGURE: Annual trends: Stratification phenology vs. year####
####This is a composite figure that shows the start of stratification, end of stratification, peak of stratifcation, and length (grey bars)

#Create yhat for date of stratification onset
AnnualData.Predicted<-AnnualData%>%dplyr::select(Year)%>%
  mutate(StartOfStratification_day_yhat=Year*as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="StartOfStratification_Day")%>%dplyr::select(Sens_Slope))+as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="StartOfStratification_Day")%>%dplyr::select(Sens_Intercept)))%>%
  mutate(LengthOfStratification_day_yhat=Year*as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="LengthOfStrat_days")%>%dplyr::select(Sens_Slope))+as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="LengthOfStrat_days")%>%dplyr::select(Sens_Intercept)))%>%
  mutate(MixingAction_gigaJday_yhat=Year*as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="MixingAction_gigaJday")%>%dplyr::select(Sens_Slope))+as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="MixingAction_gigaJday")%>%dplyr::select(Sens_Intercept)))%>%
  mutate(MaxStability_Jperm2_yhat=Year*as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="MaxStability_Jperm2")%>%dplyr::select(Sens_Slope))+as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="MaxStability_Jperm2")%>%dplyr::select(Sens_Intercept)))

#**Phenology of stratification plot####
gg.stratificationPhenology<-ggplot()+
  geom_segment(data=AnnualData,aes(x=Year,xend=Year,y=StartOfStratification_Day,yend=EndOfStratification_Day),col="grey")+
  geom_point(data=AnnualData,aes(x=Year,y=StartOfStratification_Day),shape=21,color="black",fill="#42AB9A",size=2)+
  geom_line(data=AnnualData.Predicted,aes(x=Year,y=StartOfStratification_day_yhat),color="grey50",lty=1)+
  geom_point(data=AnnualData,aes(x=Year,y=EndOfStratification_Day),shape=21,color="black",fill="#42AB9A",size=2)+
  geom_point(data=AnnualData,aes(x=Year,y=DayMaxStability),col="red",shape=1,size=2)+
  #ylim(320,90)+
  scale_y_reverse(lim=c(320,90),breaks=seq(300,100,by=-50))+
  scale_x_continuous(limit=c(1984,2018),breaks=seq(1985,2015,by=5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))+
  xlab("Year")+
  ylab(bquote(Day~of~Year))

#**Length of stratification plot####
gg.lengthOfStratification<-ggplot()+
  geom_point(data=AnnualData,aes(x=Year,y=LengthOfStrat_days),shape=21,color="black",fill="#42AB9A",size=2)+
  geom_line(data=AnnualData.Predicted,aes(x=Year,y=LengthOfStratification_day_yhat),color="grey50",lty=1)+
  scale_y_continuous(limit=c(160,220),breaks=seq(160,220,by=20))+
  scale_x_continuous(limit=c(1984,2018),breaks=seq(1985,2015,by=5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))+
  xlab("Year")+
  ylab(bquote(Length~of~strat.~(days)))

#**Max stratification plot####
gg.maxStratification<-ggplot()+
  geom_point(data=AnnualData,aes(x=Year,y=MaxStability_Jperm2),shape=21,color="black",fill="#42AB9A",size=2)+
  geom_line(data=AnnualData.Predicted,aes(x=Year,y=MaxStability_Jperm2_yhat),color="grey50",lty=1)+
  scale_y_continuous(limit=c(400,610),breaks=seq(400,600,by=50))+
  scale_x_continuous(limit=c(1984,2018),breaks=seq(1985,2015,by=5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))+
  xlab("Year")+
  ylab(bquote(Max.~stability~(J~m^-2)))

#**Summer Secchi plot####
gg.summerSecchi<-ggplot()+
  geom_point(data=AnnualData,aes(x=Year,y=SecchiDepth_Summer_m),shape=21,color="black",fill="#42AB9A",size=2)+
  #geom_line(data=AnnualData.Predicted,aes(x=Year,y=MaxStability_Jperm2_yhat),color="grey50",lty=1)+ #NOT SIGNIFICANT SO LEAVING THIS OUT
  scale_y_continuous(trans="reverse",limit=c(7,3.5),breaks=seq(7,4,by=-1))+
  scale_x_continuous(limit=c(1984,2018),breaks=seq(1985,2015,by=5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))+
  xlab("Year")+
  ylab(bquote(Secchi~depth~(m)))

#**Mixing action solo plot####
gg.mixingAction<-ggplot()+
  geom_point(data=AnnualData,aes(x=Year,y=MixingAction_gigaJday),shape=21,color="black",fill="#42AB9A",size=2)+
  geom_line(data=AnnualData.Predicted,aes(x=Year,y=MixingAction_gigaJday_yhat),color="grey50",lty=1)+
  ylim(3.0,4.6)+
  scale_x_continuous(limit=c(1984,2018),breaks=seq(1985,2015,by=10))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))+
  xlab("Year")+
  ylab(bquote(Mixing~Action~(GJ~day)))

#***Export Phenology of stratification plot####
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StratificationPhenology.jpg",
     width = 3.3, height = 2.6,
     units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

gg.stratificationPhenology

dev.off()  
par(op)

#***Export Length of stratification plot####
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-LengthOfStratification.jpg",
     width = 3.3, height = 2.6,
     units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

gg.lengthOfStratification

dev.off()  
par(op)

#***Export Mixing action solo plot####
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-MixingAction.jpg",
     width = 3.3, height = 2.6,
     units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

gg.mixingAction

dev.off()  
par(op)

#***Export Max stratification plot####
ggsave("figures/exploratory/MTCC-ExploratoryFig-MaxStability.jpg",plot=gg.maxStratification,width=3.3,height=2.6,device="jpeg")

#***Export Max stratification plot####
ggsave("figures/exploratory/MTCC-ExploratoryFig-SummerSecchi.jpg",plot=gg.summerSecchi,width=3.3,height=2.6,device="jpeg")


#***arrange into a multi-panel plot####
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
gg.3panel<-ggarrange(gg.stratificationPhenology+
                       theme(axis.title.x=element_blank(),
                             axis.text.x=element_blank(),
                             axis.ticks.x=element_blank(),
                             plot.margin=unit(c(2,0.5,0,1), "lines"),
                             axis.ticks.length.x = unit(0, "pt"))+
                       labs(x=NULL, title=NULL),
                          gg.lengthOfStratification+
                       theme(axis.title.x=element_blank(),
                             axis.text.x=element_blank(),
                             axis.ticks.x=element_blank(),
                             plot.margin=unit(c(0,0.5,0,1), "lines"),
                             axis.ticks.length.x = unit(0, "pt"))+
                       labs(x=NULL, title=NULL),
                          gg.mixingAction+
                       theme(plot.margin=unit(c(0,0.5,2,1), "lines")),
                          nrow=3)
gg.3panel
#top,right,bottom,left 

#***Export stratification 3 panel plot####
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StratificationThreePanel.jpg",
     width = 3.5, height = 2.3*3,
     units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

gg.3panel

dev.off()  
par(op)

#IAO 2021-06-09. Export for NYS Fed powerpoint
#3 panel horizontally
gg.3panel.horizontal<-ggarrange(gg.stratificationPhenology+
                                  
                       theme(
                         # axis.title.x=element_blank(),
                         #     axis.text.x=element_blank(),
                             # axis.ticks.x=element_blank(),
                             plot.margin=unit(c(2,0.5,0,1), "lines")
                             # axis.ticks.length.x = unit(0, "pt")
                             ),
                     gg.lengthOfStratification+
                       theme(
                         # axis.title.x=element_blank(),
                             # axis.text.x=element_blank(),
                             # axis.ticks.x=element_blank(),
                         axis.title.x=element_text(),
                             plot.margin=unit(c(0,0.5,0,1), "lines")
                             # axis.ticks.length.x = unit(0, "pt")
                         ),
                     gg.mixingAction+
                       theme(plot.margin=unit(c(0,0.5,2,1), "lines")),
                     ncol=3)
gg.3panel.horizontal
ggsave("figures/exploratory/NYS_Fed_talk_stratificatioPhenology.jpg",
       plot=gg.3panel.horizontal,width=10,height=4,
       units="in",device="jpeg", dpi=300)



#*Ridgelines of Schmidt Stability####
####This could also be one of those cool stacked histogram figures: density ridge plots
####http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/
####Would have to cut DailyInterpol by the start and end of strat (stability.cutoff) then
####https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

#**Subset out 1985 data to graph ridgeline
#temp1985<-DailyInterpol%>%select(year,dayofyear,stability_Jperm2)%>%filter(stability_Jperm2>Stability.cutoff)%>%filter(!is.na(stability_Jperm2))%>%filter(year==1985)
#ggplot(temp1,aes(x=dayofyear,y=rep(1,length(temp1$dayofyear)),height=stability_Jperm2))+
#geom_ridgeline()

#**Overlapping ridgelines - subset out everything without 2014
Ridgelines.DF<-DailyInterpol%>%dplyr::select(year,dayofyear,stability_Jperm2)%>%filter(stability_Jperm2>Stability.cutoff)%>%filter(!is.na(stability_Jperm2))%>%filter(year!=2014)%>%filter(year!=1997)
#ggplot(Ridgelines.DF,aes(x=dayofyear,y=year,height=stability_Jperm2,group=year))+
#geom_ridgeline(fill="lightblue")

#**Here the heights are automatically scale such that the highest ridgeline just 
#touches the one above it at scale=1
#ggplot(Ridgelines.DF,aes(x=dayofyear,y=year,height=stability_Jperm2,group=year))+
#geom_density_ridges(stat="identity",scale=5,fill="lightblue")

#Gridlines for the plot
segment_data = data.frame(
  x = c(seq(100,300,by=50)),
  xend = c(seq(100,300,by=50)), 
  y = rep(1985,length(seq(100,300,by=50))),
  yend =rep(2017,length(seq(100,300,by=50)))
)

#**Make them a little pretty
figure.ridgelines<-ggplot()+
  geom_hline(yintercept=c(seq(1985,2017,by=1)),col="light grey")+
  geom_segment(data=segment_data,aes(x=x,y=y,xend=xend,yend=yend),col="light grey")+
  #geom_vline(xintercept=c(seq(100,300,by=50)),col="light grey")+
  geom_density_ridges_gradient(data=Ridgelines.DF,aes(x=dayofyear,y=year,height=stability_Jperm2,group=year,fill=stat(height)),stat="identity",scale=2)+
  scale_fill_viridis_c(option="C",guide = guide_colorbar(frame.colour = "black",frame.linewidth = 1.25,ticks = TRUE,ticks.colour="white",ticks.linewidth=2))+
  ylab("Year")+
  xlab("Day of Year")+
  theme_bw()+  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),plot.margin=unit(c(2,0.2,2,0.2), "lines"),legend.box.background = element_blank(),plot.background = element_blank(),legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,0,-10,-5),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))+
  labs(fill=expression(atop("St",paste("(J m"^-2*")"))))+
  scale_y_continuous(limit=c(1985,2019),breaks=c(seq(1985,2015,by=5)))

p <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StabilityRidgelines.jpg", width = 4, height = 5.5, units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

figure.ridgelines

dev.off()
par(op)

#**Heat map alternative to the ridgeline plot####

#**Overlapping ridgelines - subset out everything without 2014, 1997
heatmap.DF<-DailyInterpol%>%dplyr::select(year,dayofyear,stability_Jperm2)%>%filter(!is.na(stability_Jperm2))%>%filter(year!=2014)%>%filter(year!=1997)

#**Create data frame with year and the day of year for all years with data from start of stratification to end####
strat.tempDF<-AnnualData%>%dplyr::select(Year,StartOfStratification_Day,EndOfStratification_Day)%>%
  rename(year=Year)%>%
  filter(year!=2014)%>%filter(year!=1997)%>%
  group_by(year)%>%
  do(data.frame(year=.$year, dayofyear=seq(.$StartOfStratification_Day,.$EndOfStratification_Day,by=1)))

#**Join the temporary df with the heatmap.DF to get all the stabilities for each year####
heatmap.DF<-left_join(strat.tempDF,heatmap.DF,by=c("year","dayofyear"))

#**Make the heatmap with geom_tile####
figure.heatmap<-ggplot(data=heatmap.DF,aes(x=dayofyear,y=year,fill=stability_Jperm2))+
         geom_tile()+
  scale_fill_viridis_c(option="C",guide = guide_colorbar(frame.colour = "black",frame.linewidth = 1.25,ticks = TRUE,ticks.colour="white",ticks.linewidth=2))+
  scale_x_continuous(limits=c(93,315))+
  labs(fill=expression(atop("St",paste("(J m"^-2*")"))))+
  ylab("Year")+
  xlab("Day of Year")+
  theme_bw()+  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),plot.margin=unit(c(2,0.2,2,0.2), "lines"),legend.box.background = element_blank(),plot.background = element_blank(),legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,0,-10,-5),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))
ggsave("figures/exploratory/MTCC-ExploratoryFig-StabilityHeatmap.jpg",plot=figure.heatmap, width = 4, height = 5.5,device="jpeg")


#*Modify the right panel with panel letters 
panelLetter.reverse <- data.frame(
  xpos = c(-Inf),
  ypos =  c(-Inf),
  hjustvar = c(-0.5) ,
  vjustvar = c(1.5))

panelLetter.normal <- data.frame(
  xpos = c(-Inf),
  ypos =  c(Inf),
  hjustvar = c(-0.5) ,
  vjustvar = c(1.5))

gg.right.panel<-ggarrange(gg.stratificationPhenology+
                            theme(axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank(),
                                  plot.margin=unit(c(2,0.5,0,1), "lines"),
                                  axis.ticks.length.x = unit(0, "pt"))+
                            labs(x=NULL, title=NULL)+
                            geom_text(data=panelLetter.reverse,
                                      aes(x=xpos,
                                          y=ypos,
                                          hjust=hjustvar,
                                          vjust=vjustvar,
                                          label="b",
                                          fontface="bold")),
                     gg.lengthOfStratification+
                       theme(axis.title.x=element_blank(),
                             axis.text.x=element_blank(),
                             axis.ticks.x=element_blank(),
                             plot.margin=unit(c(0,0.5,0,1), "lines"),
                             axis.ticks.length.x = unit(0, "pt"))+
                       labs(x=NULL, title=NULL)+
                       geom_text(data=panelLetter.normal,
                                 aes(x=xpos,
                                     y=ypos,
                                     hjust=hjustvar,
                                     vjust=vjustvar,
                                     label="c",
                                     fontface="bold")),
                     gg.mixingAction+
                       theme(plot.margin=unit(c(0,0.5,2,1), "lines"))+
                       geom_text(data=panelLetter.normal,
                                 aes(x=xpos,y=ypos,hjust=hjustvar,
                                     vjust=vjustvar,label="d",
                                     fontface="bold")),
                     nrow=3)

#**EXPLORATORY FIGURE: Plot into ridgelines on the left; three panel dot plots on the right####
op <- par(no.readonly = TRUE)

# jpeg("figures/MTCC-FigX-StabilityFourPanels.jpg", width = 6.6, height = 6.9, units = "in",res = 600)
# jpeg("figures/fig3.StabilityFourPanels.jpg", width = 6.6, height = 6.9, units = "in",res = 600)

fig3<-grid.arrange(
  figure.ridgelines+geom_text(data=panelLetter.normal,aes(x=xpos,
                                                          y=ypos,
                                                          hjust=hjustvar,
                                                          vjust=vjustvar,
                                                          label="a",
                                                          fontface="bold")),
  gg.right.panel,
  widths = c(1, 1),
  layout_matrix = rbind(c(1, 2),
                        c(1, 2))
)

fig3
##exportPDF
ggsave("figures/PdfsForSubmission/MTCC-Fig3-StabilityFourPanels.pdf", plot=fig3, width=6.6, height=6.9,units="in", dpi=300)

dev.off()
par(op)


#**Make a horizontal ridgelines plot####
#Reverse the day of year now that it is on the y axis
figure.ridgelines.horizontal<-figure.ridgelines+scale_x_reverse(breaks=c(seq(300,100,by=-50)))+coord_flip()

op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StabilityRidgelinesHorizontal.jpg", width = 6.6, height = 4, units = "in",res = 300)

figure.ridgelines.horizontal

dev.off()
par(op)

#**Create a 4 panel plot of all the phenology stratification figures####
#Modify each figure to mush the panels together, remove x axis ticks and labels
#The top panel (ridgeline) is 2x as big as the other ones
#Create double line y axis labels
gg.horizontal.panels<-ggarrange(figure.ridgelines.horizontal+xlab("Stratification period\n(day of year)")
                                +theme(axis.title.x=element_blank(),
                                                                  axis.text.x=element_blank(),
                                                                  axis.ticks.x=element_blank(),
                                                                  plot.margin=unit(c(2,2,0,2), "lines"),
                                                                  axis.ticks.length.x = unit(0, "pt"))+
                                  labs(y=NULL, title=NULL)+
                                  geom_text(data=panelLetter.reverse,
                                            aes(x=xpos,
                                                y=ypos,
                                                hjust=hjustvar,
                                                vjust=vjustvar,
                                                label="a")), 
                          gg.stratificationPhenology+ylab("Start, Peak, End\n(day of year)")+
                            theme(axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank(),
                                  plot.margin=unit(c(0,2,0,2), "lines"),
                                  axis.ticks.length.x = unit(0, "pt"))+
                            labs(x=NULL, title=NULL)+
                            geom_text(data=panelLetter.reverse,
                                      aes(x=xpos,
                                          y=ypos,
                                          hjust=hjustvar,
                                          vjust=vjustvar,
                                          label="b"))+
                            scale_x_continuous(limit=c(1985,2019),breaks=seq(1985,2015,by=5)),
                          gg.lengthOfStratification+ylab("Length\n(days)")+
                            theme(axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank(),
                                  plot.margin=unit(c(0,2,0,2), "lines"),
                                  axis.ticks.length.x = unit(0, "pt"))+
                            labs(x=NULL, title=NULL)+
                            geom_text(data=panelLetter.normal,
                                      aes(x=xpos,
                                          y=ypos,
                                          hjust=hjustvar,
                                          vjust=vjustvar,
                                          label="c"))+
                            scale_x_continuous(limit=c(1985,2019),breaks=seq(1985,2015,by=5)),
                          gg.mixingAction+ylab("Mixing Action\n(GJ day)")+
                            theme(plot.margin=unit(c(0,2,2,2), "lines"))+
                            geom_text(data=panelLetter.normal,
                                      aes(x=xpos,y=ypos,hjust=hjustvar,
                                          vjust=vjustvar,label="d"))+
                            scale_x_continuous(limit=c(1985,2019),breaks=seq(1985,2015,by=5)),
                          nrow=4,heights=c(2,1,1,1))

op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StabilityRidgelinesHorizontalWOtherPanels.jpg", width = 6.6, height = 7, units = "in",res = 600)

gg.horizontal.panels

dev.off()
par(op)


AnnualData %>%
  select(Year, IceInDayofYear, IceOutDayofYear, LengthOfIceCover_days) %>%
  pivot_longer(-Year) %>%
  ggplot(aes(x=Year, y=value))+
  geom_point(shape=21, fill="white", size=3)+
  geom_line(size=0.5, color="black")+
  facet_wrap(.~name,  nrow=3, scales="free_y")

