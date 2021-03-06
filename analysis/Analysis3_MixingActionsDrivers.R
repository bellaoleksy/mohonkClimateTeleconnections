#==============================================================================#
# Script created by Isabella Oleksy <bellaoleksy@gmail.com and David Richardson <richardsond@newpaltz.edu>
# Script created in R version 4.0.5 (2021-03-31)
#==============================================================================#

##This file includes all the time series models for predicting temporal variability in Mixing Action

#Load libraries
if(!require(forecast)){install.packages("huxtable")}
if(!require(forecast)){install.packages("magrittr")}
if(!require(forecast)){install.packages("forecast")}
if(!require(forecast)){install.packages("officer")}
if(!require(forecast)){install.packages("flextable")}
if(!require(egg)){install.packages("egg")} #tag_facet ftn
install.packages("viridis")
library(viridis)
library(lubridate)
library(tidyverse)
library(forecast)
library(urca)
library(fracdiff)
library(tseries)
library(magrittr)
library(huxtable) #Pretty tables
library(officer) #exporting pretty tables to word
library(flextable) #exporting pretty tables to word
library(GGally) #correlation matrices
library(ggcorrplot)
library(ggpubr) #for ggarrange()
library(egg)

# Mixing Action TS - 1 predictor models--------------------------------------------------------

# ~ MA , xreg="Year" ------------------------------------------------------


arimaFit.Year<-auto.arima(AnnualData$MixingAction_gigaJday,
                          xreg=c(AnnualData$Year),
                     # xreg=cbind(AnnualData$GlobalTempAnomoly_C,
                                # AnnualData$Year),
                     seasonal=FALSE,allowdrift = FALSE,
                     stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. You need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component 

arimaFit.Year #display the model that was fit

Yearcoef<-as.numeric(arimaFit.Year$coef["xreg"])#extracts the xreg coefficient

MA_residuals<-as.numeric(arimaFit.Year$residuals) #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
Year_pdq<-paste0(as.numeric(arimaFit.Year$arma[1]),sep=",",
                       as.numeric(arimaFit.Year$arma[6]),sep=",",
                       as.numeric(arimaFit.Year$arma[2]))

#extract aicc value
Yearaicc<-as.numeric(arimaFit.Year$aicc)

# ~ MA , xreg="GlobalTempAnom_C" ------------------------------------------------------


arimaFit.GlobalTempAnomoly<-auto.arima(AnnualData$MixingAction_gigaJday,
                          xreg=c(AnnualData$GlobalTempAnomoly_C),
                          seasonal=FALSE,allowdrift = FALSE,
                          stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.GlobalTempAnomoly #display the model that was fit

GlobalTempAnomolycoef<-as.numeric(arimaFit.GlobalTempAnomoly$coef["xreg"])#extracts the xreg coefficient

GlobalTempAnomolyresid<-as.numeric(arimaFit.GlobalTempAnomoly$residuals)

# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
GlobalTempAnomoly_pdq<-paste0(as.numeric(arimaFit.GlobalTempAnomoly$arma[1]),sep=",",
                       as.numeric(arimaFit.GlobalTempAnomoly$arma[6]),sep=",",
                       as.numeric(arimaFit.GlobalTempAnomoly$arma[2]))

#extract aicc value
arimaFit.GlobalTempAnomoly$aicc

# ~ MA , xreg="NAO_Spring" ------------------------------------------------------


arimaFit.NAO_Spring<-auto.arima(AnnualData$MixingAction_gigaJday,
                                       xreg=c(AnnualData$NAO_Spring),
                                       seasonal=FALSE,allowdrift = FALSE,
                                       stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.NAO_Spring #display the model that was fit

NAO_Springcoef<-as.numeric(arimaFit.NAO_Spring$coef["xreg"])#extracts the xreg coefficient
NAO_Springcoef<-as.numeric(arimaFit.NAO_Spring$coef["intercept"])#extracts the xreg coefficient

# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p [1] is the autoregressive terms, d [6] is the integrative, and q [2] is the moving average
NAO_Spring_pdq<-paste0(as.numeric(arimaFit.NAO_Spring$arma[1]),sep=",",
                       as.numeric(arimaFit.NAO_Spring$arma[6]),sep=",",
                       as.numeric(arimaFit.NAO_Spring$arma[2]))
                       


#extract aicc value
arimaFit.NAO_Spring$aicc

# ~ MA , xreg="NAO_Summer" ------------------------------------------------------


arimaFit.NAO_Summer<-auto.arima(AnnualData$MixingAction_gigaJday,
                                xreg=c(AnnualData$NAO_Summer),
                                seasonal=FALSE,allowdrift = FALSE,
                                stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.NAO_Summer #display the model that was fit

arimaFit.NAO_Summer$coef["xreg"] #extracts the xreg coefficient

# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
#Gets the (p,d,q) model fit where p [1] is the autoregressive terms, d [6] is the integrative, and q [2] is the moving average
NAO_Summer_pdq<-paste0(as.numeric(arimaFit.NAO_Summer$arma[1]),sep=",",
                       as.numeric(arimaFit.NAO_Summer$arma[6]),sep=",",
                       as.numeric(arimaFit.NAO_Summer$arma[2]))

#extract aicc value
arimaFit.NAO_Summer$aicc

# ~ MA , xreg="NAO_StratifiedPeriod" ------------------------------------------------------


arimaFit.NAO_StratifiedPeriod<-auto.arima(AnnualData$MixingAction_gigaJday,
                                xreg=c(AnnualData$NAO_StratifiedPeriod),
                                seasonal=FALSE,allowdrift = FALSE,
                                stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.NAO_StratifiedPeriod #display the model that was fit

arimaFit.NAO_StratifiedPeriod$coef["xreg"] #extracts the xreg coefficient

# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
NAO_StratifiedPeriod_pdq<-paste0(as.numeric(arimaFit.NAO_StratifiedPeriod$arma[1]),sep=",",
                       as.numeric(arimaFit.NAO_StratifiedPeriod$arma[6]),sep=",",
                       as.numeric(arimaFit.NAO_StratifiedPeriod$arma[2]))

#extract aicc value
arimaFit.NAO_StratifiedPeriod$aicc


# ~ MA , xreg="NAO_SpringMixed" ------------------------------------------------------


arimaFit.NAO_SpringMixed<-auto.arima(AnnualData$MixingAction_gigaJday,
                                          xreg=c(AnnualData$NAO_SpringMixed),
                                          seasonal=FALSE,allowdrift = FALSE,
                                          stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.NAO_SpringMixed #display the model that was fit

arimaFit.NAO_SpringMixed$coef["xreg"] #extracts the xreg coefficient

# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
NAO_SpringMixed_pdq<-paste0(as.numeric(arimaFit.NAO_SpringMixed$arma[1]),sep=",",
                                 as.numeric(arimaFit.NAO_SpringMixed$arma[6]),sep=",",
                                 as.numeric(arimaFit.NAO_SpringMixed$arma[2]))
#extract aicc value
arimaFit.NAO_SpringMixed$aicc


# ~ MA , xreg="ENSO_Spring" ------------------------------------------------------


arimaFit.ENSO_Spring<-auto.arima(AnnualData$MixingAction_gigaJday,
                                     xreg=c(AnnualData$ENSO_Spring),
                                     seasonal=FALSE,allowdrift = FALSE,
                                     stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.ENSO_Spring #display the model that was fit

arimaFit.ENSO_Spring$coef["xreg"] #extracts the xreg coefficient

# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
ENSO_Spring_pdq<-paste0(as.numeric(arimaFit.ENSO_Spring$arma[1]),sep=",",
                            as.numeric(arimaFit.ENSO_Spring$arma[6]),sep=",",
                            as.numeric(arimaFit.ENSO_Spring$arma[2]))

#extract aicc value
arimaFit.ENSO_Spring$aicc


# ~ MA , xreg="ENSO_Summer" ------------------------------------------------------


arimaFit.ENSO_Summer<-auto.arima(AnnualData$MixingAction_gigaJday,
                                 xreg=c(AnnualData$ENSO_Summer),
                                 seasonal=FALSE,allowdrift = FALSE,
                                 stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.ENSO_Summer #display the model that was fit

arimaFit.ENSO_Summer$coef["xreg"] #extracts the xreg coefficient


# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
ENSO_Summer_pdq<-paste0(as.numeric(arimaFit.ENSO_Summer$arma[1]),sep=",",
                        as.numeric(arimaFit.ENSO_Summer$arma[6]),sep=",",
                        as.numeric(arimaFit.ENSO_Summer$arma[2]))

#extract aicc value
arimaFit.ENSO_Summer$aicc

# ~ MA , xreg="ENSO_MEI_Spring_mean" ------------------------------------------------------


arimaFit.ENSO_MEI_Spring<-auto.arima(AnnualData$MixingAction_gigaJday,
                                 xreg=c(AnnualData$ENSO_MEI_Spring_mean),
                                 seasonal=FALSE,allowdrift = FALSE,
                                 stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.ENSO_MEI_Spring #display the model that was fit

arimaFit.ENSO_MEI_Spring$coef["xreg"] #extracts the xreg coefficient

# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
ENSO_MEI_Spring_pdq<-paste0(as.numeric(arimaFit.ENSO_MEI_Spring$arma[1]),sep=",",
                        as.numeric(arimaFit.ENSO_MEI_Spring$arma[6]),sep=",",
                        as.numeric(arimaFit.ENSO_MEI_Spring$arma[2]))

#extract aicc value
arimaFit.ENSO_MEI_Spring$aicc


# ~ MA , xreg="ENSO_MEI_Summer_mean" ------------------------------------------------------


arimaFit.ENSO_MEI_Summer<-auto.arima(AnnualData$MixingAction_gigaJday,
                                 xreg=c(AnnualData$ENSO_MEI_Summer_mean),
                                 seasonal=FALSE,allowdrift = FALSE,
                                 stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.ENSO_MEI_Summer #display the model that was fit

arimaFit.ENSO_MEI_Summer$coef["xreg"] #extracts the xreg coefficient


# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
ENSO_MEI_Summer_pdq<-paste0(as.numeric(arimaFit.ENSO_MEI_Summer$arma[1]),sep=",",
                        as.numeric(arimaFit.ENSO_MEI_Summer$arma[6]),sep=",",
                        as.numeric(arimaFit.ENSO_MEI_Summer$arma[2]))

#extract aicc value
arimaFit.ENSO_MEI_Summer$aicc



# ~ MA , xreg="ENSO_MEI_Winter_mean" ------------------------------------------------------


arimaFit.ENSO_MEI_Winter_mean<-auto.arima(AnnualData$MixingAction_gigaJday,
                                     xreg=c(AnnualData$ENSO_MEI_Winter_mean),
                                     seasonal=FALSE,allowdrift = FALSE,
                                     stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.ENSO_MEI_Winter_mean #display the model that was fit

arimaFit.ENSO_MEI_Winter_mean$coef["xreg"] #extracts the xreg coefficient


# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
ENSO_MEI_Winter_mean_pdq<-paste0(as.numeric(arimaFit.ENSO_MEI_Winter_mean$arma[1]),sep=",",
                            as.numeric(arimaFit.ENSO_MEI_Winter_mean$arma[6]),sep=",",
                            as.numeric(arimaFit.ENSO_MEI_Winter_mean$arma[2]))

#extract aicc value
arimaFit.ENSO_MEI_Winter_mean$aicc



# ~ MA , xreg="ENSO_MEI_Winter_sum" ------------------------------------------------------


arimaFit.ENSO_MEI_Winter_sum<-auto.arima(AnnualData$MixingAction_gigaJday,
                                     xreg=c(AnnualData$ENSO_MEI_Winter_sum),
                                     seasonal=FALSE,allowdrift = FALSE,
                                     stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.ENSO_MEI_Winter_sum #display the model that was fit

arimaFit.ENSO_MEI_Winter_sum$coef["xreg"] #extracts the xreg coefficient


# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
ENSO_MEI_Winter_sum_pdq<-paste0(as.numeric(arimaFit.ENSO_MEI_Winter_sum$arma[1]),sep=",",
                            as.numeric(arimaFit.ENSO_MEI_Winter_sum$arma[6]),sep=",",
                            as.numeric(arimaFit.ENSO_MEI_Winter_sum$arma[2]))

#extract aicc value
arimaFit.ENSO_MEI_Winter_sum$aicc



# ~ MA , xreg="strat. period Secchi" ------------------------------------------------------


arimaFit.secchi<-auto.arima(AnnualData$MixingAction_gigaJday,
                                 xreg=c(AnnualData$SecchiDepth_StratifiedPeriod_m),
                                 seasonal=FALSE,allowdrift = FALSE,
                                 stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.secchi #display the model that was fit

arimaFit.secchi$coef["xreg"] #extracts the xreg coefficient


# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
secchi_Summer_pdq<-paste0(as.numeric(arimaFit.secchi$arma[1]),sep=",",
                        as.numeric(arimaFit.secchi$arma[6]),sep=",",
                        as.numeric(arimaFit.secchi$arma[2]))

#extract aicc value
arimaFit.secchi$aicc


# Compile model output  ---------------------------------------------------------

#Column for response variable (Mixing Action in all models)
Response <- "Mixing Action"

#Vector of predictors used in models 
Predictor <- c("Year",
               "GlobalTempAnomoly",
               "NAO_Spring",
               "NAO_Summer",
               "NAO_StratifiedPeriod",
               "NAO_SpringMixed",
               "ENSO_Spring",
               "ENSO_Summer",
               "ENSO_MEI_Spring",
               "ENSO_MEI_Summer",
               "ENSO_MEI_Winter_mean",
               "ENSO_MEI_Winter_sum")

#Pull all xreg interecepts
Yearint<-as.numeric(arimaFit.Year$coef["intercept"])
GlobalTempAnomolyint<-as.numeric(arimaFit.GlobalTempAnomoly$coef["intercept"])
NAO_Springint<-as.numeric(arimaFit.NAO_Spring$coef["intercept"])
NAO_Summerint<-as.numeric(arimaFit.NAO_Summer$coef["intercept"])
NAO_StratifiedPeriodint<-as.numeric(arimaFit.NAO_StratifiedPeriod$coef["intercept"])
NAO_SpringMixedint<-as.numeric(arimaFit.NAO_SpringMixed$coef["intercept"])
ENSO_Springint<-as.numeric(arimaFit.ENSO_Spring$coef["intercept"])
ENSO_Summerint<-as.numeric(arimaFit.ENSO_Summer$coef["intercept"])
ENSO_MEI_Springint<-as.numeric(arimaFit.ENSO_MEI_Spring$coef["intercept"])
ENSO_MEI_Summerint<-as.numeric(arimaFit.ENSO_MEI_Summer$coef["intercept"])
ENSO_MEI_Winter_meanint<-as.numeric(arimaFit.ENSO_MEI_Winter_mean$coef["intercept"])
ENSO_MEI_Winter_sumint<-as.numeric(arimaFit.ENSO_MEI_Winter_sum$coef["intercept"])





#Vector of xreg coefficients
int <- c(Yearint,
          GlobalTempAnomolyint,
          NAO_Springint,
          NAO_Summerint,
          NAO_StratifiedPeriodint,
          NAO_SpringMixedint,
          ENSO_Springint,
          ENSO_Summerint,
          ENSO_MEI_Springint,
          ENSO_MEI_Summerint,
          ENSO_MEI_Winter_meanint,
          ENSO_MEI_Winter_sumint)

#Pull all xreg coefficients
Yearcoef<-as.numeric(arimaFit.Year$coef["xreg"])
GlobalTempAnomolycoef<-as.numeric(arimaFit.GlobalTempAnomoly$coef["xreg"])
NAO_Springcoef<-as.numeric(arimaFit.NAO_Spring$coef["xreg"])
NAO_Summercoef<-as.numeric(arimaFit.NAO_Summer$coef["xreg"])
NAO_StratifiedPeriodcoef<-as.numeric(arimaFit.NAO_StratifiedPeriod$coef["xreg"])
NAO_SpringMixedcoef<-as.numeric(arimaFit.NAO_SpringMixed$coef["xreg"])
ENSO_Springcoef<-as.numeric(arimaFit.ENSO_Spring$coef["xreg"])
ENSO_Summercoef<-as.numeric(arimaFit.ENSO_Summer$coef["xreg"])
ENSO_MEI_Springcoef<-as.numeric(arimaFit.ENSO_MEI_Spring$coef["xreg"])
ENSO_MEI_Summercoef<-as.numeric(arimaFit.ENSO_MEI_Summer$coef["xreg"])
ENSO_MEI_Winter_meancoef<-as.numeric(arimaFit.ENSO_MEI_Winter_mean$coef["xreg"])
ENSO_MEI_Winter_sumcoef<-as.numeric(arimaFit.ENSO_MEI_Winter_sum$coef["xreg"])

#Vector of xreg coefficients
coef <- c(Yearcoef,
          GlobalTempAnomolycoef,
          NAO_Springcoef,
          NAO_Summercoef,
          NAO_StratifiedPeriodcoef,
          NAO_SpringMixedcoef,
          ENSO_Springcoef,
          ENSO_Summercoef,
          ENSO_MEI_Springcoef,
          ENSO_MEI_Summercoef,
          ENSO_MEI_Winter_meancoef,
          ENSO_MEI_Winter_sumcoef)

#Pull all AICc values
YearAICC<-as.numeric(arimaFit.Year$aicc)
GlobalTempAnomolyAICC<-as.numeric(arimaFit.GlobalTempAnomoly$aicc)
NAO_SpringAICC<-as.numeric(arimaFit.NAO_Spring$aicc)
NAO_SummerAICC<-as.numeric(arimaFit.NAO_Summer$aicc)
NAO_StratifiedPeriodAICC<-as.numeric(arimaFit.NAO_StratifiedPeriod$aicc)
NAO_SpringMixedAICC<-as.numeric(arimaFit.NAO_SpringMixed$aicc)
ENSO_SpringAICC<-as.numeric(arimaFit.ENSO_Spring$aicc)
ENSO_SummerAICC<-as.numeric(arimaFit.ENSO_Summer$aicc)
ENSO_MEI_SpringAICC<-as.numeric(arimaFit.ENSO_MEI_Spring$aicc)
ENSO_MEI_SummerAICC<-as.numeric(arimaFit.ENSO_MEI_Summer$aicc)
ENSO_MEI_Winter_meanAICC<-as.numeric(arimaFit.ENSO_MEI_Winter_mean$aicc)
ENSO_MEI_Winter_sumAICC<-as.numeric(arimaFit.ENSO_MEI_Winter_sum$aicc)

#vector of AICc values
AICc<-c(YearAICC, GlobalTempAnomolyAICC, NAO_SpringAICC, NAO_SummerAICC,
        NAO_StratifiedPeriodAICC, NAO_SpringMixedAICC, ENSO_SpringAICC,
        ENSO_SummerAICC,ENSO_MEI_SpringAICC,ENSO_MEI_SummerAICC,
        ENSO_MEI_Winter_meanAICC,ENSO_MEI_Winter_sumAICC
        )

#Vector of all p,d,q data
pdq <- c(Year_pdq,
         GlobalTempAnomoly_pdq,
         NAO_Spring_pdq,
         NAO_Summer_pdq,
         NAO_StratifiedPeriod_pdq,
         NAO_SpringMixed_pdq,
         ENSO_Spring_pdq,
         ENSO_Summer_pdq,
         ENSO_MEI_Spring_pdq,
         ENSO_MEI_Summer_pdq,
         ENSO_MEI_Winter_mean_pdq,
         ENSO_MEI_Winter_sum_pdq)

#Combine to make a dataframe of model output 
TS_dataframe<-data.frame(Response, Predictor, AICc, int, coef, pdq)



# --> Export table to .docx -----------------------------------------------

TS_dataframe_hux <- 
  hux(TS_dataframe) %>% 
  arrange(AICc) %>%
  add_colnames() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE) 


# quick_html(TS_dataframe_hux, file = 'figures/tables/timeseries_table.html')
# quick_docx(TS_dataframe_hux, file = 'figures/tables/TableS2_1factormodel.docx')
#Manualy combine with TableS2_2factormodel.docx outside of R. See below. 

# Mixing Action TS - 2 predictor models--------------------------------------------------------

# ~ MA , xreg="GlobalTemp + NAO_Spring" ------------------------------------------------------


arimaFit.GlobalTempAnomoly.NAO_Spring<-auto.arima(AnnualData$MixingAction_gigaJday,
                          xreg=cbind(AnnualData$GlobalTempAnomoly_C,AnnualData$NAO_Spring),
                          seasonal=FALSE,allowdrift = FALSE,
                          stationary=TRUE)

arimaFit.GlobalTempAnomoly.NAO_Spring #display the model that was fit

#extract aicc value
GlobalTempAnomoly.NAO_Springaicc<-as.numeric(arimaFit.GlobalTempAnomoly.NAO_Spring$aicc)

#Combine model output into DF
#Column for response variable (Mixing Action in all models)
Response <- "Mixing Action"

#Vector of predictors used in models 
Predictors <- "GlobalTempAnomoly + NAO_Spring"

#Pull all xreg coefficients and intercepts
coef_globaltemp<-as.numeric(arimaFit.GlobalTempAnomoly.NAO_Spring$coef["xreg1"])#extracts the xreg coefficient
coef_NAOspring<-as.numeric(arimaFit.GlobalTempAnomoly.NAO_Spring$coef["xreg2"])#extracts the xreg coefficient
int<-as.numeric(arimaFit.GlobalTempAnomoly.NAO_Spring$coef["intercept"])#common intercept


#Pull all AICc values
AICC<-as.numeric(arimaFit.GlobalTempAnomoly.NAO_Spring$aicc)

#Pull pdq values
#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
pdq<-paste0(as.numeric(arimaFit.GlobalTempAnomoly.NAO_Spring$arma[1]),sep=",",
                                         as.numeric(arimaFit.GlobalTempAnomoly.NAO_Spring$arma[6]),sep=",",
                                         as.numeric(arimaFit.GlobalTempAnomoly.NAO_Spring$arma[2]))

#Combine to make a dataframe of model output 
TS_dataframe_2factormodels<-data.frame(Response, Predictors, AICC,
                                       int, coef_globaltemp, coef_NAOspring,
                                       pdq)



  # --> Export table to .docx -----------------------------------------------

TS_dataframe_2factormodels_hux<-
  hux(TS_dataframe_2factormodels) %>% 
  add_colnames() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE) 
  
  # quick_docx(TS_dataframe_2factormodels_hux, file = 'figures/tables/TableS2_2factormodel.docx')
  


# > Export manuscript plot ------------------------------------------------

theme_MS <- function () { 
  theme_base(base_size=10) %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, size=1.0),
      plot.title=element_text(face="plain",hjust=0.5),
      plot.subtitle = element_text(color="dimgrey", hjust=0, size=10),
      panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
      strip.background = element_blank(),
      strip.text.y = element_text(size=10, angle=270),
      strip.text.x = element_text(size=10)
    )
}



AnnualData$fittedMA<-as.numeric(arimaFit.GlobalTempAnomoly.NAO_Spring$fitted)


two_panel<-AnnualData %>%
  select(fittedMA,NAO_Spring,GlobalTempAnomoly_C)%>%
  rename("Global temperature anomaly (ºC)"="GlobalTempAnomoly_C",
         "Spring NAO index"="NAO_Spring")%>%
  pivot_longer(-1, names_to="predictorID", values_to="predictorValue")%>%
  ggplot(aes(y=fittedMA, x=predictorValue))+
  geom_jitter(shape=21,size=2, color="black", fill="#42AB9A", width=0.05)+
  scale_fill_viridis_c(guide = "colourbar")+
  theme_MS() + 
  ylab("Mixing action (GJ day)")+
  theme(strip.placement = "outside", #strip placement on outside of x-axis labels and ticks
        panel.spacing=grid::unit(0,"lines"), #squish panels together
        strip.text.x = element_text(margin = margin(0,0,0.2,0, "cm")), #increase strip box size
        axis.title.x=element_blank(),
        panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  facet_wrap(.~predictorID, scales="free_x", switch="x")
two_panel<-tag_facet(two_panel, open=NULL, close=NULL)
two_panel

# ggsave("figures/manuscript/fig4.MixingAction.png", width=5, height=3,units="in", dpi=300)



# > Figure S8. Teleconnections -----------------------------


breaks_fun <- function(y) {
  if (max(y) < 1.5) {
    seq(-1, 2, 0.3)
  } else {
    seq(-1, 2, 0.5)
  }
}



AnnualDataLong<-AnnualData %>%
  select(Year,
         NAO_Spring,           
         NAO_Summer,           
         NAO_StratifiedPeriod ,
         NAO_SpringMixed,      
         ENSO_Spring  ,        
         ENSO_Summer) %>%
  pivot_longer(-Year, names_to="variable", values_to = "value") 

#Label predictors
AnnualDataLong$variable <- factor(AnnualDataLong$variable, 
                                    labels=c('ENSO (Spring)',
                                             'ENSO (Summer)',
                                             'NAO (Spring)',
                                             'NAO (Spring mixed-period)',
                                             'NAO (Stratified period)',
                                             'NAO (Summer)'))

AnnualDataLong %>%
  ggplot(aes(x=Year, y=value))+
  geom_line(size=0.5)+
  geom_point(size=2.5,shape=21,color="black", fill="grey50")+
  facet_wrap(.~variable, nrow=3, scales="free_y")+
  labs(y="Standardized departures (unitless)",
       legend="Teleconnection variable")+
  scale_x_continuous(breaks=seq(1985,2020,5))+
  scale_y_continuous(breaks = breaks_fun)+
  # scale_color_colorblind()+
  # scale_fill_colorblind() + 
  theme_MS()+
  theme_MS()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # axis.line = element_line(colour = "black"),
        legend.position="none",
        panel.spacing=grid::unit(0,"lines"))## squash panels together
# 
# ggsave("figures/supplementary/figureS8.NAOandENSO_timeseries.jpg", width=6, height=7,units="in", dpi=300)
# ggsave("figures/supplementary/figureS8.NAOandENSO_timeseries.pdf", width=6, height=7,units="in", dpi=300)


#Declutter Global Environment

rm(list = ls()[grep("arimaFit.", ls())])
rm(list = ls()[grep("TS_", ls())])
rm(list = ls()[grep("two_panel", ls())])
rm(list = ls()[grep("ENSO_", ls())])  
rm(list = ls()[grep("GlobalTemp", ls())])  
rm(list = ls()[grep("NAO_", ls())])   
rm(list = ls()[grep("Year", ls())])  
rm(list = ls()[grep("AIC", ls())])   
rm(list = ls()[grep("coef", ls())])   

