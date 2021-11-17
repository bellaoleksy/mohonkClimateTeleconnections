#==============================================================================#
# Script created by Isabella Oleksy <bellaoleksy@gmail.com and David Richardson <richardsond@newpaltz.edu>
# Script created in R version 4.0.5 (2021-03-31)
#==============================================================================#

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


# Figure 3. Summer surface and deep water temperature correlations ---------------------------------------

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


####** Export MS figure

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
# ggsave("figures/manuscript/Figure3.correlations.png", plot=full.figure, width=6.5, height=5,units="in", dpi=600)






# Figure S10. Pretty correlation table  ----------------------------------------


#Create correlation tibble
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
data$responseID <- factor(data$responseID,
                           levels=c(
                                    "DeepWaterTemp_Summer_degC",
                                    "SurfaceWaterTemp_Summer_degC"),
                           labels=c(
                                    "Summer DWT (ºC)",
                                    "Summer SWT (ºC)"))

# To apply the test to all lakes, we need the corresponding groupings. Therefore,
# we use the group_by() function for indicating the two groups: predictorID and responseID
# In addition, we apply the nest() function with the aim of creating lists of tables
# nested per row. 
data_nest <- group_by(data, predictorID, responseID) %>% nest()
data_nest

#Function for correlation test in tidy format
cor_fun <- function(df) cor.test(df$predictorValue, df$responseValue, method = "pearson") %>% tidy()


data_nest <- mutate(data_nest, model = map(data, cor_fun))

#To undo the list of tables in each row of our table, first we eliminate the column
#with the data and then simply we can apply the unnest() function.
corr <- select(data_nest, -data) %>% unnest(cols = c(model))


# Export correlation table for supplement


#Column with p-value significance or not
corr <- mutate(corr, sig = ifelse(p.value <=0.05, "Sig.", "Non Sig."))


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
  
  
  # ggsave("figures/supplementary/figureS10.CorrelationMatrix.png", width=5, height=6,units="in", dpi=600)
  
  
  #Declutter Global Environment
  rm(list = ls()[grep("gg.", ls())])
  rm(list = ls()[grep("plot", ls())])
  rm(list = ls()[grep("data", ls())])
