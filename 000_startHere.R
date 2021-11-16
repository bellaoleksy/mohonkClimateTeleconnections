#Call in all the appropriate functions, pull in data, pull in script that summarizes all the data, and run the first analysis

#Loads all functions####
source('script/helperFunctions.R')
#QAQC's data####
source('script/loadDataAndQAQC.R')
#Pulls in data available from EDI####
# Mohonk Preserve, Belardo, C., Feldsine, N., Huth, P., Long, E. C., Napoli, M., Oleksy, I. A., Richardson, D. C., Smiley, D., & Thompson, J. (2020). Weekly and high frequency temperature profile data and Secchi depth, Mohonk Lake, NY, USA, 1985 to 2017 [Data set]. Environmental Data Initiative. https://doi.org/10.6073/PASTA/7B67399344129AFC63CD57E99E778160
source('script/readEDI.R')
#Run the munging file####
#This modifies and summarizes all the data
source('script/dataMunging.R')



#Run the first analysis ####
#Export manuanalyses figures 1-2 and figure S9 from this file
source('Analysis1_OverallTrendsAndSummaryFigures.R')
 

#Lake trend analysis####
#Includes PCA on variables that influence surface and hypolimnion temperatures. 
#Exports Figure S11.
source('Analysis2_LakeTrends.R')


#Mixing action analysis####
#Includes ARIMA models for mixing action (manuscript figure 4)
#as well as Figure S8 (teleconnection time series)
source('Analysis3_MixingActionsDrivers.R')


#Mixing action and temperature profiles methods check####
#Includes some response to reviewer concerns which end up in the supplementary materials
#Figures S3, S4, S6, and S7
source('Analysis4_MethodsCheckProfilesMixingAction.R')


#Correlations between surface water temperature or deep water temperature and ####
#several covariates. Includes Figure 3 and Figure S10.
source('Analysis5_correlations.R')





