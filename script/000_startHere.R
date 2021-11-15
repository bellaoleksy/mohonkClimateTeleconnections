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

#Run the first analysis - sometimes this is necessary for using some of the outputs like Sen's slopes####
source('script/MTCC_Analysis1_OverallTrendsAndSummaryFigures.R')

