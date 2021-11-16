
# Run these helper scripts first -----------------------------------------------


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


# Analyses ---------------------------------------------------------------------

### Analysis 1 -----------------------------------------------------------------
# We assessed interannual temporal trends for a variety of lake and air temperature metrics:
# post-ice, spring, and summer air, surface water and deep water;
# stratification onset, turnover, and length, maximum stability,
# maximum stability day, and mixing action (Table S1).  
# Using code derived from both ‘zyp’ and ‘trend’ R packages, we calculated Theil-Sen’s estimator
# (Theil-Sen’s slope) as the median slope from all finite pairwise slopes (Bronaugh & Consortium, 2019; Pohlert, 2020).
# For visualization, we calculated the intercept as the median intercept using
# the Theil-Sen’s slope and all pairwise points. To test if a trend was significant,
# we used the Mann-Kendall rank based z-score and compared the p-value from that z-score to α=0.05. 

# Export manuscripts figures 1-2, figure S9, Table S1 from this file

source('Analysis1_OverallTrendsAndSummaryFigures.R')
 

### Analysis 2 -----------------------------------------------------------------
# We identified six local factors that could drive interannual variability in summer SWT and DWT.
# For SWT, we included summer mean daily air temperature, summer total daily precipitation,
# spring mixed-period SWT, stratification onset DOY, length of the mixed-period,
# and Secchi depth as a proxy for transparency. For deep water, we included summer SWT,
# ice off day of the year, spring mixed-period DWT, stratification onset DOY, length of the mixed-period,
# and transparency. For each set of factors, we carried out non-parametric Spearman correlations
# with either summer SWT or DWT, accounting for the multiple comparisons (n=6 for each) with a
# Bonferroni correction (α=0.05). To reduce each of these sets of variables down to orthogonal variables,
#we ran principal component analyses using the prcomp function in R (see supplement for details).

#Exports Figure S11.
source('Analysis2_LakeTrends.R')


### Analysis 3  ----------------------------------------------------------------
# After preliminary analyses indicated changes in MA over time, we tested how well
# global temperature anomalies and large-scale climatic oscillations could explain the observed patterns.
# We considered antecedent winter, spring, and summer NAO and ENSO indices (Table S1, Figure S8).
# We built time series models using the auto.arima() function in the R ‘forecast’ package
# for each individual predictor to address possible autocorrelation in the time series (Hyndman & Khandakar, 2008).
#Model selection was based on the Akaike information criterion (AICc); the top two models were combined into a final model. 

# Exports figure 4 and figure S8 (teleconnection time series)
source('Analysis3_MixingActionsDrivers.R')

### Analysis 4  ----------------------------------------------------------------
#Mixing action and temperature profiles methods check 
#Includes some response to reviewer concerns which end up in the supplementary materials
#Figures S3, S4, S6, and S7
source('Analysis4_MethodsCheckProfilesMixingAction.R')

### Analysis 5  ----------------------------------------------------------------
#Correlations between surface water temperature or deep water temperature and
#several covariates. Includes Figure 3 and Figure S10.
source('Analysis5_correlations.R')





