#Plot 2 rows by 3 columns
# First row is significant correlations with summer surface water temp
# Second row is significant correlations with summer deep water temp


library(scales)
library(ggplot2)
library(patchwork)
library(stringr)

# Set theme ---------------------------------------------------------------


theme_MS <- function () { 
  theme_base(base_size=10) %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, size=1.0),
      plot.title=element_text(face="plain",hjust=0.5),
      plot.subtitle = element_text(color="dimgrey", hjust=0, size=10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.y = element_text(size=10, angle=270),
      strip.text.x = element_text(size=10),
      panel.spacing=grid::unit(0,"lines"),
      axis.ticks.length = unit(0.1, "cm")
    )
}




# Create correlation tibble ----------------------------------------



data <- AnnualData %>%
  mutate(lengthMixedPeriod=StartOfStratification_Day-IceOutDayofYear) %>%
  filter(lengthMixedPeriod >0)%>%
  dplyr::select(SurfaceWaterTemp_Summer_degC,DeepWaterTemp_Summer_degC,MixingAction_gigaJday,
                AirTemp_Summer_degC,SecchiDepth_Summer_m,SecchiDepth_StratifiedPeriod_m,
                IceOutDayofYear,DeepWaterTemp_SpringMixed_degC,lengthMixedPeriod,
                Precip_Summer_mm, SurfaceWaterTemp_SpringMixed_degC, LengthOfStrat_days, MaxStability_Jperm2 
                ) %>%
  pivot_longer(-(1:2), names_to = "predictorID", values_to = "predictorValue") %>%
  dplyr::select(predictorID, predictorValue, SurfaceWaterTemp_Summer_degC:DeepWaterTemp_Summer_degC) %>%
  pivot_longer(-(1:2), names_to="responseID", values_to="responseValue")
data
data$predictorID <- as.factor(as.character(data$predictorID))
data$responseID <- as.factor(as.character(data$responseID))

###I fixed the factor order here so it's good for go future use (like for a talk or whatever)
####-- IAO 2020-08-14

levels(data$predictorID)
# #Specify levels for predictor ID 
data$predictorID <- factor(data$predictorID,
                                    levels=c("AirTemp_Summer_degC",
                                             "MixingAction_gigaJday",
                                             "IceOutDayofYear",
                                             "lengthMixedPeriod",                
                                             "LengthOfStrat_days",
                                             "SecchiDepth_StratifiedPeriod_m",
                                             "SecchiDepth_Summer_m",
                                             "MaxStability_Jperm2",
                                             "DeepWaterTemp_SpringMixed_degC",
                                             "SurfaceWaterTemp_SpringMixed_degC",
                                             "Precip_Summer_mm"
                                             ),
                                    labels=c("Summer air T (ºC)",
                                             "Mixing Action (GJ day)",
                                             "Ice out (DOY)",
                                             "Length mixed-period (days)",
                                             "Length of stratification (days)",
                                             "Strat. period secchi depth (m)",
                                             "Summer secchi depth (m)",
                                             "Max. Schmidt stability (J m-2)",
                                             "Spring mixed-period DWT (ºC)",
                                             "Spring mixed-period SWT (ºC)",
                                             "Total summer precip (mm)"
                                             ))
levels(data$predictorID) #check that it worked
# 
levels(data$responseID)
data$responseID <- factor(data$responseID,
                           levels=c(
                                    "DeepWaterTemp_Summer_degC",
                                    "SurfaceWaterTemp_Summer_degC"
                                    ),
                           labels=c(
                                    "Summer DWT (ºC)",
                                    "Summer SWT (ºC)"
                                    ))
levels(data$responseID) #check that it worked

# 

# To apply the test to all lakes, we need the corresponding groupings. Therefore,
# we use the group_by() function for indicating the two groups: predictorID and responseID
# In addition, we apply the nest() function with the aim of creating lists of tables
# nested per row. 
data_nest <- group_by(data, predictorID, responseID) %>% nest()
data_nest

#Function for correlation test in tidy format
cor_fun <- function(df) cor.test(df$predictorValue, df$responseValue, method = "pearson") %>% tidy()


data_nest <- mutate(data_nest, model = map(data, cor_fun))
data_nest
str(slice(data_nest, 1))

#To undo the list of tables in each row of our table, first we eliminate the column
#with the data and then simply we can apply the unnest() function.
corr <- select(data_nest, -data) %>% unnest()


########** Export correlation table for supplement#########################


#Column with p-value significance or not
corr <- mutate(corr, sig = ifelse(p.value <=0.05, "Sig.", "Non Sig."))


  # ggplot()+
  # geom_tile(data = corr,
  #           aes(predictorID, responseID, fill = estimate),
  #           size = 1,
  #           colour = "white")+
  # geom_tile(data = filter(corr, sig == "Sig."),
  #           aes( predictorID, responseID,),
  #           size = 1,
  #           colour = "black",
  #           fill = "transparent")+
  # geom_text(data = corr,
  #           aes( predictorID, responseID, label = round(estimate, 2),
  #                fontface = ifelse(sig == "Sig.", "bold", "plain")))+
  # scale_fill_gradient2(breaks = seq(-1, 1, 0.2))+
  # labs(x = "", y = "", fill = "")+
  # theme_minimal()+
  # theme(panel.grid.major = element_blank(),
  #       panel.border = element_blank(),
  #       panel.background = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.text.x = element_text(angle = 45, hjust = 1))+
  #   guides(fill = guide_colourbar(barwidth = 1, barheight = 18,
  #                                 label.theme = element_text(size=10,colour = "black", angle = 0)))
  # ggsave("figures/figS10.CorrelationMatrix.png", width=8, height=6,units="in", dpi=600)


  ggplot()+
    geom_tile(data = corr,
              aes( responseID, predictorID,fill = estimate),
              size = 1,
              colour = "white")+
    geom_tile(data = filter(corr, sig == "Sig."),
              aes( responseID,predictorID),
              size = 1,
              colour = "black",
              fill = "transparent")+
    geom_text(data = corr,
              aes( responseID, predictorID, label = round(estimate, 2),
                   fontface = ifelse(sig == "Sig.", "bold", "italic")))+
    scale_fill_gradient2(breaks = seq(-1, 1, 0.2))+
    labs(x = "", y = "", fill = "")+
    theme_minimal()+
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_y_discrete(limits = rev(levels(corr$predictorID)))+
    guides(fill = guide_colourbar(barwidth = 1, barheight = 23,
                                  label.theme = element_text(size=10,colour = "black", angle = 0)))
  # ggsave("figures/figS9.CorrelationMatrix.png", width=5, height=6,units="in", dpi=600)
  
# Create individual panels figure 4 ---------------------------------------

  cor.test(AnnualData$SecchiDepth_StratifiedPeriod_m, AnnualData$SurfaceWaterTemp_Summer_degC)
  
  #Correlations for reference, need to print on plot
  corr <- AnnualData %>%
    mutate(lengthMixedPeriod=StartOfStratification_Day-IceOutDayofYear) %>%
    filter(lengthMixedPeriod >0)%>%
    select(SurfaceWaterTemp_Summer_degC,DeepWaterTemp_Summer_degC,MixingAction_gigaJday,
           AirTemp_Summer_degC,SecchiDepth_Summer_m,SecchiDepth_StratifiedPeriod_m,
           IceOutDayofYear,DeepWaterTemp_SpringMixed_degC,lengthMixedPeriod,
    ) %>%
    drop_na() %>%
    cor(method="pearson") %>%
    as_tibble()%>%
    rownames_to_column() %>%
    # filter(rowname %in% c("1","2")) %>%
    mutate(corr_variable= 
             ifelse(rowname %in% c(1), "SurfaceWaterTemp_Summer_degC", 
                    ifelse(rowname %in% c(2), "DeepWaterTemp_Summer_degC",
                           ifelse(rowname %in% c(3), "MixingAction_gigaJday",NA)))) %>%
    drop_na()%>%
    select(-rowname)


plotA<-ggplot(data=AnnualData, aes(y=SurfaceWaterTemp_Summer_degC, x=AirTemp_Summer_degC))+
  geom_point(shape=21, colour="black",size=2, fill="#80cdc1") + 
  ylab("Summer surface\nwater temperature (ºC)")+
  xlab("Summer air temperature (ºC)")+
  coord_cartesian(ylim=c(21,25), xlim=c(19,24))+
  scale_y_continuous(breaks=pretty_breaks(n=6))+
  scale_x_continuous(breaks=pretty_breaks(n=6))+
  theme_MS()+
  theme(plot.margin=unit(c(0,0,0.5,0.2), "lines"))+
  geom_text(aes(fontface="bold"),
            x=23.5, y = 21,
            label = "r = 0.80",
            size = 10/.pt)


#Surface temp v. secchi
plotB<- ggplot(data=AnnualData, aes(y=SurfaceWaterTemp_Summer_degC, x=SecchiDepth_Summer_m))+
  geom_point(shape=21, colour="black",size=2, fill="#80cdc1") + 
  ylab("Summer surface\nwater temperature (ºC)")+
  xlab("Summer secchi depth (m)")+
  coord_cartesian(ylim=c(21,25))+
  scale_y_continuous(breaks=pretty_breaks(n=6))+
  scale_x_continuous(breaks=pretty_breaks(n=4))+
  # scale_x_continuous(limits=c(1984,2018),breaks=seq(1985, 2020, 10))+
  theme_MS()+
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=unit(c(0,0,0.5,0), "lines"),
        axis.ticks.length.y = unit(0, "pt"))+
  geom_text(aes(fontface="bold"),
            x=6.5, y = 21,
            label = "r = 0.52",
            size = 10/.pt)

#Surface temp v. deep temp
plotC<-ggplot(data=AnnualData, aes(y=SurfaceWaterTemp_Summer_degC, x=DeepWaterTemp_Summer_degC))+
  geom_point(shape=21, colour="black",size=2, fill="#80cdc1") + 
  ylab("Summer surface\nwater temperature (ºC)")+
  xlab("Summer deep\nwater temperature (ºC)")+
  coord_cartesian(ylim=c(21,25))+
  scale_y_continuous(breaks=pretty_breaks(n=6))+
  scale_x_continuous(breaks=pretty_breaks(n=5))+
  theme_MS()+
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=unit(c(0,0.2,0.5,0), "lines"),
        axis.ticks.length.y = unit(0, "pt"))+
  geom_text(aes(fontface="italic"),
            x=7.8, y = 21,
            label = "r = -0.18",
            size = 10/.pt)


#Deep temp v. ice off
plotD<-ggplot(data=AnnualData, aes(y=DeepWaterTemp_Summer_degC, x=IceOutDayofYear))+
  geom_jitter(shape=21, colour="black",size=2, fill="#018571") + 
  # geom_smooth(method="lm", se=FALSE, size=0.5, color="grey50")+
  # facet_grid(vars(Variable), vars(Season), scales="free_y", margins=FALSE)+
  # scale_fill_manual(values=TempColors,
  #                   name="Variable")+
  ylab("Summer deep\nwater temperature (ºC)")+
  xlab("Ice out\n(day of year)")+
  coord_cartesian(ylim=c(5.5,8))+
  scale_y_continuous(breaks=pretty_breaks(n=6))+
  scale_x_continuous(breaks=pretty_breaks(n=5))+
  theme_MS()+
  theme(plot.margin=unit(c(0,0,0,0.2), "lines"))+
  geom_text(aes(fontface="bold"),
            x=105, y = 5.5,
            label = "r = -0.48",
            size = 10/.pt)

#Summer deep temp v. spring deep temp
plotE<-ggplot(data=AnnualData, aes(y=DeepWaterTemp_Summer_degC, x=DeepWaterTemp_SpringMixed_degC))+
  geom_point(shape=21, colour="black",size=2, fill="#018571") + 
  ylab("Summer deep\nwater temperature (ºC)")+
  xlab("Spring mixed-period\ndeep water temperature (ºC)")+
  coord_cartesian(ylim=c(5.5,8))+
  scale_y_continuous(breaks=pretty_breaks(n=6))+
  scale_x_continuous(breaks=pretty_breaks(n=5))+
  theme_MS()+
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=unit(c(0,0,0,0), "lines"),
        axis.ticks.length.y = unit(0, "pt"))+
  geom_text(aes(fontface="bold"),
            x=6, y = 5.5,
            label = "r = 0.77",
            size = 10/.pt)



#Summer deep temp v. length of mixed period (StartOfStratification_Day - IceOutDayofYear)
plotG<-AnnualData %>%
  mutate(lengthMixedPeriod=StartOfStratification_Day-IceOutDayofYear) %>%
  filter(lengthMixedPeriod >0)%>%
ggplot(aes(y=DeepWaterTemp_Summer_degC, x=lengthMixedPeriod))+
  geom_point(shape=21, colour="black",size=2, fill="#018571") + 
  ylab("Summer deep\nwater temperature (ºC)")+
  xlab("Length of mixed-period (days)")+
  coord_cartesian(ylim=c(5.5,8))+
  scale_y_continuous(breaks=pretty_breaks(n=6))+
  scale_x_continuous(breaks=pretty_breaks(n=5))+
  theme_MS()+
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=unit(c(0,0.2,0,0), "lines"),
        axis.ticks.length.y = unit(0, "pt"))+
  geom_text(aes(fontface="bold"),
            x=40, y = 5.5,
            label = "r = 0.78",
            size = 10/.pt)


####** Export MS figure######

#annotate
panelLetter.normal <- data.frame(
  xpos = c(-Inf),
  ypos =  c(Inf),
  hjustvar = c(-0.5) ,
  vjustvar = c(1.5))


full.figure<-ggarrange(plotA+
                            geom_text(data=panelLetter.normal,
                                      aes(x=xpos,
                                          y=ypos,
                                          hjust=hjustvar,
                                          vjust=vjustvar,
                                          label="a",
                                          fontface="bold")),
                          plotB+
                            geom_text(data=panelLetter.normal,
                                      aes(x=xpos,
                                          y=ypos,
                                          hjust=hjustvar,
                                          vjust=vjustvar,
                                          label="b",
                                          fontface="bold")),
                          plotC+
                            geom_text(data=panelLetter.normal,
                                      aes(x=xpos,y=ypos,hjust=hjustvar,
                                          vjust=vjustvar,label="c",
                                          fontface="bold")),
                           plotD+
                             geom_text(data=panelLetter.normal,
                                       aes(x=xpos,
                                           y=ypos,
                                           hjust=hjustvar,
                                           vjust=vjustvar,
                                           label="d",
                                           fontface="bold")),
                           plotE+
                             geom_text(data=panelLetter.normal,
                                       aes(x=xpos,
                                           y=ypos,
                                           hjust=hjustvar,
                                           vjust=vjustvar,
                                           label="e",
                                           fontface="bold")),
                           plotG+

                             geom_text(data=panelLetter.normal,
                                       aes(x=xpos,y=ypos,hjust=hjustvar,
                                           vjust=vjustvar,label="f",
                                           fontface="bold")),
                                  ncol=3, nrow=2)
full.figure
ggsave("figures/fig4.correlations.png", plot=full.figure, width=6.5, height=5,units="in", dpi=600)




# Reviewer Responses ------------------------------------------------------

#Reviewer #1:
# My only concern is the referral to global temperature anomaly and the NAO index to account for the observed mixing action. What is the driving force behind these correlations? In this study, the authors only refer to temperature, but not to other meteorological factors, such as precipitation and wind. For example, the NAO index is not only a measure for changes in temperature, but also for precipitation. Seeing that rain storm intensity and duration can also alter the thermal lake regime, even during the stratified period, I am wondering how important precipitation is to explain differences of, e.g., the deep water temperature of Lake Mohonk? Although this was mentioned in the Discussion (l 260), it would have been important to include precipitation data to this analysis because weather affects lakes basically by temperature and precipitation (and also wind). What would be the role of precipitation on lake stratification? Seeing that the Secchi depth during the stratified period increased considerably during the study period, how did precipitation affect lake transparency, lake temperature and stratification?

#Relationship between NAO and precipitation
AnnualData %>%
  select(NAO_Spring, NAO_Summer,Precip_Spring_mm, Precip_Summer_mm, MixingAction_gigaJday) %>%
  drop_na() %>%
  cor(method="pearson") %>%
  as_tibble()%>%
  rownames_to_column() %>%
  # filter(rowname %in% c("1","2")) %>%
  mutate(corr_variable= 
           ifelse(rowname %in% c(1), "NAO_Spring", 
                  ifelse(rowname %in% c(2), "NAO_Summer",
                         ifelse(rowname %in% c(3), "Precip_Spring_mm",
                                ifelse(rowname %in% c(4), "Precip_Summer_mm",
                                    ifelse(rowname %in% c(5),"MixingAction_gigaJday", NA)))))) %>%
  drop_na()%>%
  select(-rowname)



# ..... export spring precip vs. NAO spring ---------------------------------


#Spring NAO vs. spring precip
springNAO_springprecip<-
  AnnualData %>%
  ggplot(aes(NAO_Spring, Precip_Spring_mm))+
  geom_point(shape=21, colour="black",size=2, fill="#80cdc1")+
  coord_cartesian(ylim=c(0,700),xlim=c(-1,1))+
  geom_text(aes(fontface="italic"),
            x=-0.75, y = 600,
            label = "r =0.26",
            size = 10/.pt)+
  xlab("Spring NAO index")+ylab("Spring precip. (mm)")
springNAO_springprecip
ggsave("figures/reviewerResponses/001_springNAO_springprecip.png", plot=springNAO_springprecip, width=4, height=4,units="in", dpi=600)


# ..... export spring precip vs. MA ---------------------------------


#Spring precip vs. mixing action
springprecip_ma<-AnnualData %>%
  ggplot(aes( Precip_Spring_mm, MixingAction_gigaJday))+
  geom_point(shape=21, colour="black",size=2, fill="#80cdc1")+
  coord_cartesian(ylim=c(2.5,5),xlim=c(0,700))+
  geom_text(aes(fontface="italic"),
            x=600, y = 4.25,
            label = "r = -0.26",
            size = 10/.pt)+
  ylab("Mixing Action (GJ day)")+xlab("Spring precip. (mm)")
springprecip_ma
ggsave("figures/reviewerResponses/002_mixingaction_springprecip.png", plot=springprecip_ma, width=4, height=4,units="in", dpi=600)

#Summer precip vs. mixing action
AnnualData %>%
  ggplot(aes(Precip_Summer_mm, MixingAction_gigaJday))+
  geom_point(shape=21, colour="black",size=2, fill="#80cdc1")+
  coord_cartesian(ylim=c(2.5,5),xlim=c(0,700))+
  geom_text(aes(fontface="italic"),
            x=600, y = 4.25,
            label = "r = -0.13",
            size = 10/.pt)

#Relationship between precipitation and deep water temperature
AnnualData %>%
  select(Precip_Spring_mm, Precip_Summer_mm, DeepWaterTemp_Summer_degC, DeepWaterTemp_StratifiedPeriod_degC,
         DeepWaterTemp_SpringMixed_degC) %>%
  drop_na() %>%
  cor(method="pearson") %>%
  as_tibble()%>%
  rownames_to_column() %>%
  # filter(rowname %in% c("1","2")) %>%
  mutate(corr_variable= 
           ifelse(rowname %in% c(1), "Precip_Spring_mm", 
                  ifelse(rowname %in% c(2), "Precip_Summer_mm",
                         ifelse(rowname %in% c(3), "DeepWaterTemp_Summer_degC",
                                ifelse(rowname %in% c(4), "DeepWaterTemp_StratifiedPeriod_degC",
                                       ifelse(rowname %in% c(5),"DeepWaterTemp_SpringMixed_degC", NA)))))) %>%
  drop_na()%>%
  select(-rowname)
#Very low correlations across the board


# ..... export spring precip vs. summer deep T ---------------------------------


#Spring precip and summer deep water T
precipspring_deepwater<-AnnualData %>%
  ggplot(aes(Precip_Spring_mm, DeepWaterTemp_Summer_degC))+
  geom_point(shape=21, colour="black",size=2, fill="#80cdc1")+
  # coord_cartesian(ylim=c(2.5,5),xlim=c(0,700))+
  geom_text(aes(fontface="italic"),
            x=600, y = 7.5,
            label = "r = -0.17",
            size = 10/.pt)+
  xlab("Spring precip. (mm)")+ylab("Deep water temp. (ºC)")
precipspring_deepwater
ggsave("figures/reviewerResponses/003_deepwaterT_springprecip.png", plot=precipspring_deepwater, width=4, height=4,units="in", dpi=600)


#Relationship between precipitation and surface water temperature
AnnualData %>%
  select(Precip_Spring_mm, Precip_Summer_mm, SurfaceWaterTemp_Summer_degC, SurfaceWaterTemp_StratifiedPeriod_degC,
         SurfaceWaterTemp_Spring_degC) %>%
  drop_na() %>%
  cor(method="pearson") %>%
  as_tibble()%>%
  rownames_to_column() %>%
  # filter(rowname %in% c("1","2")) %>%
  mutate(corr_variable= 
           ifelse(rowname %in% c(1), "Precip_Spring_mm", 
                  ifelse(rowname %in% c(2), "Precip_Summer_mm",
                         ifelse(rowname %in% c(3), "SurfaceWaterTemp_Summer_degC",
                                ifelse(rowname %in% c(4), "SurfaceWaterTemp_StratifiedPeriod_degC",
                                       ifelse(rowname %in% c(5),"SurfaceWaterTemp_Spring_degC", NA)))))) %>%
  drop_na()%>%
  select(-rowname)

#Here we do see significant correlation btwn spring precip and surface water temperature 
#during the stratified period
cor.test(AnnualData$Precip_Spring_mm, AnnualData$SurfaceWaterTemp_StratifiedPeriod_degC)
precipspring_surfaceTstratified<-AnnualData %>%
  ggplot(aes(Precip_Spring_mm, SurfaceWaterTemp_StratifiedPeriod_degC))+
  geom_point(shape=21, colour="black",size=2, fill="#80cdc1")+
  # coord_cartesian(ylim=c(2.5,5),xlim=c(0,700))+
  geom_text(aes(fontface="bold"),
            x=600, y = 20.5,
            label = "r = -0.42",
            size = 10/.pt)+
  xlab("Spring precip. (mm)")+ylab("Stratified period surface water temperature (ºC)")
precipspring_surfaceTstratified
ggsave("figures/reviewerResponses/004_precipspring_surfaceTstratified.png", plot=precipspring_surfaceTstratified, width=4, height=4,units="in", dpi=600)


cor.test(AnnualData$Precip_Spring_mm, AnnualData$DeepWaterTemp_StratifiedPeriod_degC)
precipspring_deepTstratified<-AnnualData %>%
  ggplot(aes(Precip_Spring_mm, DeepWaterTemp_StratifiedPeriod_degC))+
  geom_point(shape=21, colour="black",size=2, fill="#80cdc1")+
  # coord_cartesian(ylim=c(2.5,5),xlim=c(0,700))+
  geom_text(aes(fontface="italic"),
            x=600, y = 8.0,
            label = "r = -0.14",
            size = 10/.pt)+
xlab("Spring precip. (mm)")+ylab("Stratified period deep water temperature (ºC)")
precipspring_deepTstratified
ggsave("figures/reviewerResponses/005_precipspring_deepTstratified.png", plot=precipspring_deepTstratified, width=4, height=4,units="in", dpi=600)


cor.test(AnnualData$Precip_StratifiedPeriod_mm, AnnualData$DeepWaterTemp_StratifiedPeriod_degC)
AnnualData %>%
  ggplot(aes(Precip_StratifiedPeriod_mm, DeepWaterTemp_StratifiedPeriod_degC))+
  geom_point(shape=21, colour="black",size=2, fill="#80cdc1")+
  # coord_cartesian(ylim=c(2.5,5),xlim=c(0,700))+
  geom_text(aes(fontface="italic"),
            x=600, y = 8.0,
            label = "r = -0.15",
            size = 10/.pt)

#Investigate the relationshipsbetween secchi depth and precipitation at any timescale
Table<-AnnualData %>%
  select(SecchiDepth_Summer_m, SecchiDepth_SpringMixed_m, SecchiDepth_StratifiedPeriod_m,
         SecchiDepth_Spring_m,Precip_Spring_mm, Precip_Summer_mm, Precip_SpringMixed_mm,
         Precip_StratifiedPeriod_mm) %>%
  drop_na() %>%
  cor(method="pearson") %>%
  as_tibble()%>%
  rownames_to_column() %>%
  # filter(rowname %in% c("1","2")) %>%
  mutate(corr_variable= 
           ifelse(rowname %in% c(1), "SecchiDepth_Summer_m", 
                  ifelse(rowname %in% c(2), "SecchiDepth_SpringMixed_m",
                         ifelse(rowname %in% c(3), "SecchiDepth_StratifiedPeriod_m",
                                ifelse(rowname %in% c(4), "SecchiDepth_Spring_m",
                                       ifelse(rowname %in% c(5),"Precip_Spring_mm",
                                              ifelse(rowname %in% c(6), "Precip_Summer_mm",
                                                     ifelse(rowname %in% c(7), "Precip_SpringMixed_mm",
                                                            ifelse(rowname %in% c(8), "Precip_StratifiedPeriod_mm",NA))))))))) %>%
  drop_na()%>%
  select(-rowname)
print(Table)
Table_export<-Table %>%
  select(corr_variable, SecchiDepth_Summer_m:SecchiDepth_Spring_m) %>%
  filter(str_detect(corr_variable, "Precip")) %>%
  mutate_at(2:5, round, 2)
#Very low correlations across the board

Table_export_hux <- 
  hux(Table_export) %>% 
  arrange(corr_variable) %>%
  add_colnames() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE) 

# quick_docx(Table_export_hux, file = 'figures/reviewer_responses_secchi_correlations.docx')

#Impact of precipitation on stratification? 