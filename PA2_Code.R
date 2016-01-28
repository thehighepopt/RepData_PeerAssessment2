


setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/RepData_PeerAssessment2")
library(knitr)
library(plyr)

if (!file.exists("repdata-data-StormData.csv.bz2")) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                  destfile = "repdata-data-StormData.csv.bz2")}

    
##read.csv can read .bz2 files directly, this will take some time.
noaa_data <- read.csv("repdata-data-StormData.csv.bz2")

str(noaa_data)

##Perform data cleansing
###Trim leading and trailing blank spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
noaa_data$EVTYPE <- trim(noaa_data$EVTYPE)
noaa_data$STATE <- trim(noaa_data$STATE)
noaa_data$PROPDMGEXP <- trim(noaa_data$PROPDMGEXP)
noaa_data$CROPDMGEXP <- trim(noaa_data$CROPDMGEXP)
noaa_data$COUNTYNAME <- trim(noaa_data$COUNTYNAME)
noaa_data$BGN_AZI <- trim(noaa_data$BGN_AZI)

R <- noaa_data
###Convert factors to dates
noaa_data$BGN_DATE <- as.Date(noaa_data$BGN_DATE,format = "%m/%d/%Y %H:%M:%S")
noaa_data$END_DATE <- as.Date(noaa_data$END_DATE,format = "%m/%d/%Y %H:%M:%S")

##Convert damages to property and crops to real numbers



##Across the United States, which types of events (as indicated in the EVTYPE variable) 
##are most harmful with respect to population health?






##Across the United States, which types of events have the greatest economic consequences?




