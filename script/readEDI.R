# Package ID: edi.552.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Weekly and high frequency temperature profile data and Secchi depth, Mohonk Lake, NY, USA, 1985 to 2017.
# Data set creator:   Mohonk Preserve - Mohonk Preserve 
# Data set creator:  Christy Belardo - Mohonk Preserve 
# Data set creator:  Natalie Feldsine - Mohonk Preserve 
# Data set creator:  Paul Huth - Mohonk Preserve 
# Data set creator:  Elizabeth Long - Mohonk Preserve 
# Data set creator:  Megan Napoli - Mohonk Preserve 
# Data set creator:  Isabella Oleksy - Cary Institute of Ecosystem Studies 
# Data set creator:  David Richardson - SUNY New Paltz 
# Data set creator:  Daniel Smiley - Mohonk Preserve 
# Data set creator:  John Thompson - Mohonk Preserve 
# Contact:   Director of Conservation Science -  Mohonk Preserve  - research@mohonkpreserve.org
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/552/1/95566fff309ad5276d5d100d8dca8f4b" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Date",     
                 "Secchi_m"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1Date) 
if (class(dt1$Secchi_m)=="factor") dt1$Secchi_m <-as.numeric(levels(dt1$Secchi_m))[as.integer(dt1$Secchi_m) ]               
if (class(dt1$Secchi_m)=="character") dt1$Secchi_m <-as.numeric(dt1$Secchi_m)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Date)
summary(Secchi_m) 
# Get more details on character variables

detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/552/1/41b314b5b26b6f597c2c6c96a2c10f4d" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Date",     
                 "Temp_0m",     
                 "Temp_1m",     
                 "Temp_2m",     
                 "Temp_3m",     
                 "Temp_4m",     
                 "Temp_5m",     
                 "Temp_6m",     
                 "Temp_7m",     
                 "Temp_8m",     
                 "Temp_9m",     
                 "Temp_10m",     
                 "Temp_11m",     
                 "Temp_12m",     
                 "Temp_13m"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt2$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2Date<-as.Date(dt2$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2Date) == length(tmp2Date[!is.na(tmp2Date)])){dt2$Date <- tmp2Date } else {print("Date conversion failed for dt2$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2Date) 
if (class(dt2$Temp_0m)=="factor") dt2$Temp_0m <-as.numeric(levels(dt2$Temp_0m))[as.integer(dt2$Temp_0m) ]               
if (class(dt2$Temp_0m)=="character") dt2$Temp_0m <-as.numeric(dt2$Temp_0m)
if (class(dt2$Temp_1m)=="factor") dt2$Temp_1m <-as.numeric(levels(dt2$Temp_1m))[as.integer(dt2$Temp_1m) ]               
if (class(dt2$Temp_1m)=="character") dt2$Temp_1m <-as.numeric(dt2$Temp_1m)
if (class(dt2$Temp_2m)=="factor") dt2$Temp_2m <-as.numeric(levels(dt2$Temp_2m))[as.integer(dt2$Temp_2m) ]               
if (class(dt2$Temp_2m)=="character") dt2$Temp_2m <-as.numeric(dt2$Temp_2m)
if (class(dt2$Temp_3m)=="factor") dt2$Temp_3m <-as.numeric(levels(dt2$Temp_3m))[as.integer(dt2$Temp_3m) ]               
if (class(dt2$Temp_3m)=="character") dt2$Temp_3m <-as.numeric(dt2$Temp_3m)
if (class(dt2$Temp_4m)=="factor") dt2$Temp_4m <-as.numeric(levels(dt2$Temp_4m))[as.integer(dt2$Temp_4m) ]               
if (class(dt2$Temp_4m)=="character") dt2$Temp_4m <-as.numeric(dt2$Temp_4m)
if (class(dt2$Temp_5m)=="factor") dt2$Temp_5m <-as.numeric(levels(dt2$Temp_5m))[as.integer(dt2$Temp_5m) ]               
if (class(dt2$Temp_5m)=="character") dt2$Temp_5m <-as.numeric(dt2$Temp_5m)
if (class(dt2$Temp_6m)=="factor") dt2$Temp_6m <-as.numeric(levels(dt2$Temp_6m))[as.integer(dt2$Temp_6m) ]               
if (class(dt2$Temp_6m)=="character") dt2$Temp_6m <-as.numeric(dt2$Temp_6m)
if (class(dt2$Temp_7m)=="factor") dt2$Temp_7m <-as.numeric(levels(dt2$Temp_7m))[as.integer(dt2$Temp_7m) ]               
if (class(dt2$Temp_7m)=="character") dt2$Temp_7m <-as.numeric(dt2$Temp_7m)
if (class(dt2$Temp_8m)=="factor") dt2$Temp_8m <-as.numeric(levels(dt2$Temp_8m))[as.integer(dt2$Temp_8m) ]               
if (class(dt2$Temp_8m)=="character") dt2$Temp_8m <-as.numeric(dt2$Temp_8m)
if (class(dt2$Temp_9m)=="factor") dt2$Temp_9m <-as.numeric(levels(dt2$Temp_9m))[as.integer(dt2$Temp_9m) ]               
if (class(dt2$Temp_9m)=="character") dt2$Temp_9m <-as.numeric(dt2$Temp_9m)
if (class(dt2$Temp_10m)=="factor") dt2$Temp_10m <-as.numeric(levels(dt2$Temp_10m))[as.integer(dt2$Temp_10m) ]               
if (class(dt2$Temp_10m)=="character") dt2$Temp_10m <-as.numeric(dt2$Temp_10m)
if (class(dt2$Temp_11m)=="factor") dt2$Temp_11m <-as.numeric(levels(dt2$Temp_11m))[as.integer(dt2$Temp_11m) ]               
if (class(dt2$Temp_11m)=="character") dt2$Temp_11m <-as.numeric(dt2$Temp_11m)
if (class(dt2$Temp_12m)=="factor") dt2$Temp_12m <-as.numeric(levels(dt2$Temp_12m))[as.integer(dt2$Temp_12m) ]               
if (class(dt2$Temp_12m)=="character") dt2$Temp_12m <-as.numeric(dt2$Temp_12m)
if (class(dt2$Temp_13m)=="factor") dt2$Temp_13m <-as.numeric(levels(dt2$Temp_13m))[as.integer(dt2$Temp_13m) ]               
if (class(dt2$Temp_13m)=="character") dt2$Temp_13m <-as.numeric(dt2$Temp_13m)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Date)
summary(Temp_0m)
summary(Temp_1m)
summary(Temp_2m)
summary(Temp_3m)
summary(Temp_4m)
summary(Temp_5m)
summary(Temp_6m)
summary(Temp_7m)
summary(Temp_8m)
summary(Temp_9m)
summary(Temp_10m)
summary(Temp_11m)
summary(Temp_12m)
summary(Temp_13m) 
# Get more details on character variables

detach(dt2)               



names(dt1)
names(dt2)

MohonkWeeklyProfilesMetric <- left_join(dt1, dt2, by="Date")


