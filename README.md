# Climate Change and Teleconnections Amplify Lake Stratification With Differential Local Controls of Surface Water Warming and Deep Water Cooling

<!-- badges: start -->
[![HitCount](http://hits.dwyl.com/bellaoleksy/mohonkClimateTeleconnections.svg?style=flat-square)](http://hits.dwyl.com/bellaoleksy/mohonkClimateTeleconnections)
<!-- badges: end -->


## Welcome! :wave: 
This is a public repository for the code needed to recreate the analysis in [Oleksy &amp; Richardson 2021](https://agupubs-onlinelibrary-wiley-com.libproxy.uwyo.edu/share/6CICWHJVCWHMIUIRAH8G?target=10.1029/2020GL090959) published in Geophysical Research Letters.

> Oleksy, I. A., & Richardson, D. C. (2021). Climate Change and Teleconnections Amplify Lake Stratification With Differential Local Controls of Surface Water Warming and Deep Water Cooling. Geophysical Research Letters, 48(5), 1–11. https://doi.org/10.1029/2020GL090959

![Mohonk Lake study area](figures/manuscript/Figure0.MohonkMap.png)

## Objectives 
Here, we used a 35-year dataset of weekly, year-round temperature profiles for Mohonk Lake, NY, to evaluate local, regional, and global drivers of trends in lake thermal structure. Our objectives were twofold. First, we explored hypothesized climatic drivers at different seasonal scales and identified the mechanisms responsible for temperature changes in surface and deep waters in a small, dimictic lake (Figure S1). Second, we aimed to explain drivers of interannual variability in lake thermal structure and stratification phenology. We developed a metric to capture all details about the stratified period–including timing of onset of stratification, stratification strength, length of stratification–called mixing action, defined as the total energy required to mix a lake over an entire stratified season. We compared mixing action directly to global metrics of climate change and teleconnections because both simultaneously affect the same local weather variables. We expected mixing action to increase with global atmospheric warming, but that some of the variability from that relationship would relate to teleconnections.

## Instructions for use
* All of the helper functions (fetching data from EDI and local `data` folder, QA/QC, data manipulation) are located in the `script` folder.
* Our workflow is divided into five separate analysis scripts:
  - `Analysis1_OverallTrendsAndSummaryFigures.R`
  - `Analysis2_LakeTrends.R`
  - `Analysis3_MixingActionDrivers.R`
  - `Analysis4_MethodsCheckProfilesMixingAction.R`
  - `Analysis5_correlations.R`
* The file `000_startHere.R` is a wrapper script where you can source all the necessary scripts to recreate our analysis. 

## Data availability
Most of the data on Mohonk Lake used in this analysis is available through Environmental Data Inititative:

> Mohonk Preserve, C. Belardo, N. Feldsine, P. Huth, E.C. Long, M. Napoli, I.A. Oleksy, D.C. Richardson, D. Smiley, and J. Thompson. 2020. Weekly and high frequency temperature profile data and Secchi depth, Mohonk Lake, NY, USA, 1985 to 2017 ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/7b67399344129afc63cd57e99e778160 (Accessed 2021-11-16).

Other ancillary data used in the analysis is available in the `data` folder.

## Contact us
Please direct any inquiries to *bellaoleksy at gmail dot com*
