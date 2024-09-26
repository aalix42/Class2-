##install.packages(c("dplyr", "lubridate"))
library(dplyr)
library(lubridate)

#dplyr makes organizing data easier! 

streamh <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteinfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

#set up new column, by parsing your date 
streamh$dateF <- ymd_hm(streamh$datetime,
                        tz = "America/New_York")

year(streamh$dateF)
#to join with the site ID: site info to stream gauge height 
floods <- full_join(streamh, siteinfo, by = "siteID")



#using the select tool! 
flood_dataNumeric <- floods %>% select_if(is.numeric)


#first argument is first table, second is second

#filtering data to isolate Peace River. Filter by any condition. 
peace <- floods %>% 
  filter(siteID == 2295637)

example <- floods %>%
  filter(gheight.ft >= 10)

plot(peace$dateF, peace$gheight.ft, type = "l")
max_ht <- floods %>% 
  group_by(names) %>% 
  summarise(max_ht_ft = max(gheight.ft, na.rm = TRUE),
            mean_ft = mean(gheight.ft, na.rm = TRUE))

#prompt 3 in class
flood_date <- floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))


#homework starts here!

#homework prompt 1 
FERiver <- floods %>%
  filter(siteID == 2256500)
hist(plotOne$gheight.ft, main = "Flood Heights at Fisheating Creek at Palmdale",
     xlab = "Gauge Height (ft)")

plotTwoWRiver <- floods %>% 
  filter(siteID == 2312000)
hist(plotTwo$gheight.ft, main = "Flood Heights at Withlacoochiee River", 
     xlab = "Gauge Height (ft)")

plotThreeSantaFe <- floods %>% 
  filter(siteID == 2322500)
hist(plotThreeSantaFe$gheight.ft, main = "Flood Heights at Santa Fe River in Feet", 
     xlab = "Gauge Height (ft)")

plotFourPeaceRiver <- floods %>% 
  filter(siteID == 2295637)
hist(plotFourPeaceRiver$gheight.ft, main = "Flood Heights at Peace River", 
     xlab = "Gauge Height (ft)")

#homework prompt 2
floodDatesOne <- floods %>% 
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_dateStageOne = min(dateF))

floodDateMod <- floods %>%
  filter(gheight.ft >= moderate.ft) %>%
  group_by(names) %>% 
  summarise(min_dateStageModerate = min(dateF))

floodDateMajor <- floods %>%
  filter(gheight.ft >= major.ft) %>% 
  group_by(names) %>%
  summarise(min_dateStageMajor= min(dateF))


minDateDF <- full_join(floodDatesOne, floodDateMajor, by = 'names')
minDateDF <- full_join(minDateDF, floodDateMod, by = 'names')

#Data Frame result for prompt 2:
minDateDF

#prompt2 part two: time difference between stage three and stage one for each river in order 
TimeDifBtwn2and3 <- floodDateMajor$min_dateStageMajor - floodDateMod$min_dateStageModerate
TimeDifBtwn2and3

#time difference between stage two and stage one for each river in order 
TimeDifBtwn2and1 <- floodDateMod$min_dateStageModerate - floodDatesOne$min_dateStageOne
TimeDifBtwn2and1

#prompt3
floodMax <- floods %>% 
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise(maxHeight = max(gheight.ft-major.ft))

#answer (also in doc): Peace River 