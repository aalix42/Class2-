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

#prompt 3 
flood_date <- floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))


#homework prompt 1 
plotOne <- floods %>%
  filter(siteID == 2256500)
hist(plotOne$gheight.ft, main = "Flood Heights at Fisheating Creek at Palmdale",
     xlab = "Gauge Height (ft)")


plotTwo <- floods %>% 
  filter(siteID == 2312000)
hist(plotOne$gheight.ft, main = "Flood Heights at Withlacoochiee River", 
     xlab = "Gauge Height (ft)")

#note to self: commit to GIT