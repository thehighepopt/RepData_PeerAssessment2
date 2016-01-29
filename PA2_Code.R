


setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/RepData_PeerAssessment2")
library(knitr)
library(plyr)
library(dplyr)

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

##Capitalize some messy columns
noaa_data$PROPDMGEXP <- toupper(noaa_data$PROPDMGEXP)
noaa_data$CROPDMGEXP <- toupper(noaa_data$CROPDMGEXP)


###Convert factors to dates
noaa_data$BGN_DATE <- as.Date(noaa_data$BGN_DATE,format = "%m/%d/%Y %H:%M:%S")
noaa_data$END_DATE <- as.Date(noaa_data$END_DATE,format = "%m/%d/%Y %H:%M:%S")

##Convert damages to property and crops to real numbers
###I multiply H, M, B EXP values by 100, 1000000, etc., all other values, e.g., K, +, 5, 6, etc. are 
 ### multiplied by 1000.  Shown in the h, i code, K makes up 97-99% of all non-blank EXP values.  To
 ### keep damages accounted for I chose the most likely multiplier for unknown EXP values.

propmult <- ifelse(noaa_data$PROPDMGEXP == 'M', 1000000, 
                  ifelse(noaa_data$PROPDMGEXP == "B", 1000000000, 
                        ifelse(noaa_data$PROPDMGEXP == "H", 100, 
                               1000)))
noaa_data$PROPDMGNUMBER <- noaa_data$PROPDMG * propmult


cropmult <- ifelse(noaa_data$CROPDMGEXP == 'M', 1000000, 
                  ifelse(noaa_data$CROPDMGEXP == "B", 1000000000, 
                        ifelse(noaa_data$CROPDMGEXP == "H", 100, 
                              1000)))
noaa_data$CROPDMGNUMBER <- noaa_data$CROPDMG * cropmult

cropexpcount <- count(!is.na(noaa_data), "CROPDMGEXP")
propexpcount <- count(noaa_data, "PROPDMGEXP")
cropexpcount$Pct <- round(cropexpcount[,2] / sum(cropexpcount[2:7,2]),3)
propexpcount$Pct <- round(propexpcount[,2] / sum(propexpcount[2:17,2]),3)

cropexpcount
propexpcount

##Across the United States, which types of events (as indicated in the EVTYPE variable) 
##are most harmful with respect to population health?

pop_health <- noaa_data[,c(8,23,24)] %>% 
                group_by(EVTYPE) %>% 
                summarise_each(funs(sum))

pop_health[,2] <- round(pop_health[,2],3)
pop_health[,3] <- round(pop_health[,3],3)

pop_health$TOTINJFATAL <- pop_health$FATALITIES + pop_health$INJURIES
         
##max_fatality <- pop_health[which(pop_health[,2] == max(pop_health[,2])),1]
##max_injuries <- pop_health[which(pop_health[,3] == max(pop_health[,3])),1]
##max_combined <- pop_health[which(pop_health[,4] == max(pop_health[,4])),1]

max_fatal <- arrange(pop_health, desc(FATALITIES))[1,1:2]
max_injury <- arrange(pop_health, desc(INJURIES))[1,c(1,3)]
max_comb <- arrange(pop_health, desc(TOTINJFATAL))[1,c(1,4)]

second_place <- arrange(pop_health, desc(TOTINJFATAL))[2,c(1,4)]

pop_health_st <- noaa_data[,c(7,8,23,24)] %>% 
    group_by(STATE,EVTYPE) %>% 
    summarise_each(funs(sum))




##Across the United States, which types of events have the greatest economic consequences?




