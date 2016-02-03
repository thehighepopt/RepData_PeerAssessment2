


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

cropexpcount <- noaa_data %>% count(CROPDMGEXP, sort = TRUE)    ##count(!is.na(noaa_data), "CROPDMGEXP")
propexpcount <- noaa_data %>% count(PROPDMGEXP, sort = TRUE)
cropexpcount$Pct <- round(cropexpcount[,2] / sum(cropexpcount[2:7,2]),3)
propexpcount$Pct <- round(propexpcount[,2] / sum(propexpcount[2:17,2]),3)

crop_k <- as.data.frame(cropexpcount)[2,c(1,3)]
prop_k <- as.data.frame(propexpcount)[2,c(1,3)]

##Across the United States, which types of events (as indicated in the EVTYPE variable) 
##are most harmful with respect to population health?

pop_health <- noaa_data[,c(8,23,24)] %>% 
                group_by(EVTYPE) %>% 
                summarise_each(funs(sum)) 

pop_health$TOTHARM <- pop_health$FATALITIES + pop_health$INJURIES


max_fatal <- arrange(pop_health, desc(FATALITIES))[1,1:2]
max_injury <- arrange(pop_health, desc(INJURIES))[1,c(1,3)]
max_comb <- arrange(pop_health, desc(TOTHARM))[1,c(1,4)]

next_five_harm <- arrange(pop_health, desc(TOTHARM))[2:6,c(1,4)]

##make a plot of injuries and fatalities by year
noaa_data$YEAR <- format(noaa_data$BGN_DATE,'%Y')
pop_health_yr <- noaa_data[,c(40,23,24)] %>% 
    group_by(YEAR) %>% 
    summarise_each(funs(sum)) 

melt_pop <- melt(pop_health_yr, id.vars = "YEAR")
melt_pop[,1] <- as.numeric(as.character(melt_pop[,1]))
ggplot(data = melt_pop, aes(x=YEAR,y=value)) +
       geom_path(aes(colour = variable))

##Across the United States, which types of events have the greatest economic consequences?
econ_dmg <- noaa_data[,c(8,38,39)] %>% 
    group_by(EVTYPE) %>% 
    summarise_each(funs(sum))
    
econ_dmg$TOTDAMAGE <- econ_dmg$PROPDMGNUMBER + econ_dmg$CROPDMGNUMBER

max_property <- arrange(econ_dmg, desc(PROPDMGNUMBER))[1,1:2]
max_crop <- arrange(econ_dmg, desc(CROPDMGNUMBER))[1,c(1,3)]
max_totdmg <- arrange(econ_dmg, desc(TOTDAMAGE))[1,c(1,4)]

next_five_dmg <- arrange(econ_dmg, desc(TOTDAMAGE))[2:6,c(1,4)]

econ_dmg_st <- noaa_data[,c(7,38,39)] %>% 
    group_by(STATE) %>% 
    summarise_each(funs(sum))

econ_dmg_st$TOTAL <- (econ_dmg_st$PROPDMGNUMBER + econ_dmg_st$CROPDMGNUMBER)
econ_dmg_st <- arrange(econ_dmg_st[,c(1,4)],desc(TOTAL))[1:10,]

barplot(econ_dmg_st$TOTAL, names.arg = econ_dmg_st$STATE, ylab = "Total Damage", main = 
            "Total Damage Top 10 States")
