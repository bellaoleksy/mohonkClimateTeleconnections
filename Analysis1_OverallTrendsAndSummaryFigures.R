#R script created 20Dec2019 by DCR####
#Updated 01Feb2020 with IAO and DCR to add figures and analyses for basic trends of all variables
#Includes key manuscript figures at the bottom

#Packages####
if(!require(reshape2)){install.packages("reshape2")};library(reshape2)
if(!require(scales)){install.packages("scales")};library(scales) #for pretty_breaks()
if(!require(ggthemes)){install.packages("ggthemes")};library(ggthemes)
if(!require(ggpubr)){install.packages("ggpubr")};library(ggpubr)
if(!require(ggridges)){install.packages("ggridges")};library(ggridges)
if(!require(egg)){install.packages("egg")};library(egg)
if(!require(gridExtra)){install.packages("gridExtra")};library(gridExtra)
if(!require(huxtable)){install.packages("huxtable")};library(huxtable) #Pretty tables
if(!require(officer)){install.packages("officer")};library(officer) #exporting pretty tables to word
if(!require(flextable)){install.packages("flextable")};library(flextable) #exporting pretty tables to word



#Set theme####
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


#Extract Mohonk Bathymetry proportons ------------------------------------------
#Sum up the volume of the lake that we cover
MohonkBathy.volume%>%filter(UpperDepth_m<=13)%>%summarize(sum=sum(Volume_proportion))



#STATS: Annual trends and breakpoint (Sen-Theil slopes)-------------------------

#*Sen slopes using DCR defined function that is drawn from zyp and trend functions
#No longer need to impute values or use zyp.sen
#Deals with NA values
#Use MTCC.sensSlope(x=AnnualData$Year,y=AnnualData$VARIABLE) 
#create new df with variable name, sen slope, intercept, z, n, pvalue

#Melts Annual Data into long form using reshape2
AnnualData.long<-melt(AnnualData,id="Year")

#**Runs Theil Sen's slopes on each variable
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

#**write csv of the sens slopes summary
# write_csv(AnnualData.SensSlopeSummary,"figures/tables/MTCC-TableX-SensSlopesAllVariables.csv")

# Table S1. --------------------------------------------------------------------

AnnualData.SensSlopeSummary_hux <- 
  hux(AnnualData.SensSlopeSummary) %>% 
  add_colnames() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE) 

theme_plain(AnnualData.SensSlopeSummary_hux)
# quick_docx(AnnualData.SensSlopeSummary_hux, file = 'figures/tables/TableS1.SensSlopesAllVariables.docx')
# Commented out because I just wanted to export
# this so I could add the table to our Supplemental Materials google doc



# Figure 1 ---------------------------------------------------------------------
## Air surface and deep temperature, with spring post-ice, spring, and summer time periods, 3x3
## Plots 3x3 with a common axis for Air, Surface water, Deep water
## Prepare data for plotting
## A little complicated because we want to only draw regression lines through trends that are statistically significant at p > 0.05
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


TempColors<- c( "#a6611a", "#80cdc1","#018571") #Air, Surface water, deep water


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


##Add sen slope labels
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
               x=1995, y = 18,
              label = c('0.173°C/year',NA,'0.04°C/year',NA,NA,NA,NA,NA,NA),
              size = 3.5)+
    geom_text(data = senLabels %>% #Middle three rows
                filter(WaterSeason=="Spring Post-Ice (air)"),
              x=1995, y = 18,
              label = c(NA,NA,NA,'0.06°C/year','0.04°C/year','0.05°C/year',NA,NA,NA),
              size = 3.5)+
    geom_text(data = senLabels %>% #Last three rows
                filter(WaterSeason=="Spring Post-Ice (air)"),
              x=1995, y = 4,
              label = c(NA,NA,NA,NA,NA,NA,NA,'-0.03°C/year',"-0.05°C/year"),
              size = 3.5)

ggsave("figures/manuscript/Figure1.AirAndWaterTempTrends.jpg", width=6.6, height=5,units="in")

panel3x3
dev.off()

#Figure 2 ----------------------------------------------------------------------

#Create yhat for date of stratification onset
AnnualData.Predicted<-AnnualData%>%
  dplyr::select(Year)%>%
  mutate(StartOfStratification_day_yhat=Year*as.numeric(AnnualData.SensSlopeSummary%>%
                                                          filter(variable=="StartOfStratification_Day")%>%
                                                          dplyr::select(Sens_Slope))+as.numeric(AnnualData.SensSlopeSummary%>%
                                                                                                  filter(variable=="StartOfStratification_Day")%>%
                                                                                                  dplyr::select(Sens_Intercept)))%>%
  mutate(LengthOfStratification_day_yhat=Year*as.numeric(AnnualData.SensSlopeSummary%>%
                                                           filter(variable=="LengthOfStrat_days")%>%
                                                           dplyr::select(Sens_Slope))+as.numeric(AnnualData.SensSlopeSummary%>%
                                                                                                   filter(variable=="LengthOfStrat_days")%>%
                                                                                                   dplyr::select(Sens_Intercept)))%>%
  mutate(MixingAction_gigaJday_yhat=Year*as.numeric(AnnualData.SensSlopeSummary%>%
                                                      filter(variable=="MixingAction_gigaJday")%>%
                                                      dplyr::select(Sens_Slope))+as.numeric(AnnualData.SensSlopeSummary%>%
                                                                                              filter(variable=="MixingAction_gigaJday")%>%
                                                                                              dplyr::select(Sens_Intercept)))%>%
  mutate(MaxStability_Jperm2_yhat=Year*as.numeric(AnnualData.SensSlopeSummary%>%
                                                    filter(variable=="MaxStability_Jperm2")%>%
                                                    dplyr::select(Sens_Slope))+as.numeric(AnnualData.SensSlopeSummary%>%
                                                                                            filter(variable=="MaxStability_Jperm2")%>%
                                                                                            dplyr::select(Sens_Intercept)))

### Figure 2A. Ridgelines of Schmidt Stability ---------------------------------

# Overlapping ridgelines - subset out everything without 2014
Ridgelines.DF<-DailyInterpol%>%
  dplyr::select(year,dayofyear,stability_Jperm2)%>%
  filter(stability_Jperm2>Stability.cutoff)%>%
  filter(!is.na(stability_Jperm2))%>%
  filter(year!=2014)%>%
  filter(year!=1997)

# Gridlines for the plot
segment_data = data.frame(
  x = c(seq(100,300,by=50)),
  xend = c(seq(100,300,by=50)), 
  y = rep(1985,length(seq(100,300,by=50))),
  yend =rep(2017,length(seq(100,300,by=50)))
)

# Make them a little pretty
figure.ridgelines<-ggplot()+
  geom_hline(yintercept=c(seq(1985,2017,by=1)),col="light grey")+
  geom_segment(data=segment_data,aes(x=x,y=y,xend=xend,yend=yend),col="light grey")+
  geom_density_ridges_gradient(data=Ridgelines.DF,aes(x=dayofyear,y=year,height=stability_Jperm2,group=year,fill=stat(height)),stat="identity",scale=2)+
  scale_fill_viridis_c(option="C",guide = guide_colorbar(frame.colour = "black",frame.linewidth = 1.25,ticks = TRUE,ticks.colour="white",ticks.linewidth=2))+
  ylab("Year")+
  xlab("Day of Year")+
  theme_bw()+  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin=unit(c(2,0.2,2,0.2), "lines"),
        legend.box.background = element_blank(),plot.background = element_blank(),legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,0,-10,-5),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))+
  labs(fill=expression(atop("St",paste("(J m"^-2*")"))))+
  scale_y_continuous(limit=c(1985,2019),breaks=c(seq(1985,2015,by=5)))

### Figure 2B. Phenology of stratification plot --------------------------------
gg.stratificationPhenology<-ggplot()+
  geom_segment(data=AnnualData,aes(x=Year,xend=Year,y=StartOfStratification_Day,yend=EndOfStratification_Day),col="grey")+
  geom_point(data=AnnualData,aes(x=Year,y=StartOfStratification_Day),shape=21,color="black",fill="#42AB9A",size=2)+
  geom_line(data=AnnualData.Predicted,aes(x=Year,y=StartOfStratification_day_yhat),color="grey50",lty=1)+
  geom_point(data=AnnualData,aes(x=Year,y=EndOfStratification_Day),shape=21,color="black",fill="#42AB9A",size=2)+
  geom_point(data=AnnualData,aes(x=Year,y=DayMaxStability),col="red",shape=1,size=2)+
  scale_y_reverse(lim=c(320,90),breaks=seq(300,100,by=-50))+
  scale_x_continuous(limit=c(1984,2018),breaks=seq(1985,2015,by=5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))+
  xlab("Year")+
  ylab(bquote(Day~of~Year))

### Figure 2C. Length of stratification plot -----------------------------------
gg.lengthOfStratification<-ggplot()+
  geom_point(data=AnnualData,aes(x=Year,y=LengthOfStrat_days),shape=21,color="black",fill="#42AB9A",size=2)+
  geom_line(data=AnnualData.Predicted,aes(x=Year,y=LengthOfStratification_day_yhat),color="grey50",lty=1)+
  scale_y_continuous(limit=c(160,220),breaks=seq(160,220,by=20))+
  scale_x_continuous(limit=c(1984,2018),breaks=seq(1985,2015,by=5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))+
  xlab("Year")+
  ylab(bquote(Length~of~strat.~(days)))

### Figure 2D. Mixing action solo plot -----------------------------------------
gg.mixingAction<-ggplot()+
  geom_point(data=AnnualData,aes(x=Year,y=MixingAction_gigaJday),shape=21,color="black",fill="#42AB9A",size=2)+
  geom_line(data=AnnualData.Predicted,aes(x=Year,y=MixingAction_gigaJday_yhat),color="grey50",lty=1)+
  ylim(3.0,4.6)+
  scale_x_continuous(limit=c(1984,2018),breaks=seq(1985,2015,by=10))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))+
  xlab("Year")+
  ylab(bquote(Mixing~Action~(GJ~day)))



#Modify the right panel with panel letters 
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

# Pull together right panel (2B, C, D)
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

### Combine 2A-D & export ------------------------------------------------------

fig2<-grid.arrange(
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

fig2
##exportPDF
ggsave("figures/manuscript/Figure2.StabilityFourPanels.pdf", plot=fig2, width=6.6, height=6.9,units="in", dpi=300)



# Figure S9 --------------------------------------------------------------------
# Composite thermocline depth and stability figure vs. day of year summarizing over all years
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

# Cutoff at these days, before 75, the median is all 0.5; after 321, there is a linear increase to 1 by about 0.4 per day
# Day 321 is about the maximum median thermocline depth - probably representing mixing of the top 
lo<-75; hi<-321
# Plot composite thermocline depth
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

# Plot composite stability
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

# Modify the right panel with panel letters 
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

#**Arrange into a two panel plot
#***arrange into a multi-panel plot
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
ggsave("figures/supplementary/figureS9.StabilityCompositeFigure.jpg",plot=gg.2panel.composite, width = 3.3, height = 4.2, units = "in",dpi = 300)


#Declutter Global Environment
rm(AnnualData.SensSlopeSummary,
   AnnualData.SensSlopeSummary_hux,
   AnnualSurfaceWater,
   AnnualDeepWater,
   AnnualAir,
   AnnualTemps,
   AnnualDeepWater_noSpringPostIce,
   AnnualAir_noSpring,
   AnnualTemps_noSpringAir,
   senLabels, 
   AnnualData.Predicted,
   ThermoclineDepth.composite,
   s.slope,
   segment_data)

rm(list = ls()[grep("gg.", ls())])
rm(list = ls()[grep("DailyInterpol", ls())])
rm(list = ls()[grep("temp", ls())])
rm(list = ls()[grep("tmp", ls())])
rm(list = ls()[grep("Stability", ls())])    
rm(list = ls()[grep("stability.", ls())])    
rm(list = ls()[grep("thermocline.", ls())])    
rm(list = ls()[grep("AnnualBreakpoint", ls())])    
rm(list = ls()[grep("AnnualData_", ls())])    
rm(list = ls()[grep("fig", ls())])    
rm(list = ls()[grep("lm", ls())])    
rm(list = ls()[grep("panel", ls())])    
