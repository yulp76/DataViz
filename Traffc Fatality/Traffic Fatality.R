# Tommy Yu
# An exploratory analysis of the DoT FARS data.

library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

acc2014 <- read_sas("accident.sas7bdat")
acc2015 <- read_csv("accident.csv")
ls()
class(acc2014)
class(acc2015)

acc2014 <- mutate(acc2014, TWAY_ID2 = na_if(TWAY_ID2, ""))
table(is.na(acc2014$TWAY_ID2))
dim(acc2014)
dim(acc2015)

colnames(acc2014[!(colnames(acc2014) %in% colnames(acc2015))])
colnames(acc2015[!(colnames(acc2015) %in% colnames(acc2014))])
# Columns from 2014 that are missing from 2015: "ROAD_FNC"
# Columns from 2015 that are missing from 2014: "RUR_URB"  "FUNC_SYS" "RD_OWNER"

acc <- bind_rows(acc2014, acc2015)
count(acc, "RUR_URB")
# 30,056 NA values due to the fact that "RUR_URB" is missing from 2014 data.
# 30,056 is precisely the number of rows in acc2014.

fips <- read_csv("fips.csv")
glimpse(fips)

acc <- mutate(acc, STATE = as.character(STATE), COUNTY = as.character(COUNTY))
acc <- mutate(acc, STATE = str_pad(STATE, 2, "left", "0"), COUNTY = str_pad(COUNTY, 3, "left", "0"))
acc <- dplyr::rename(acc, StateFIPSCode = STATE, CountyFIPSCode = COUNTY)
acc <- left_join(acc, fips, by = c("StateFIPSCode", "CountyFIPSCode"))

#Exploratory Analysis - No Pipe
by_year_state <- group_by(acc, YEAR, StateName)
agg <- summarize(by_year_state, TOTAL = sum(FATALS, na.rm = TRUE))
agg_wide <- spread(agg, YEAR, TOTAL)
agg_wide <- rename(agg_wide, Year2014 = "2014", Year2015 = "2015")
agg_wide <- mutate(agg_wide, Diff_Percent = (Year2015-Year2014)/Year2014)
agg_wide <- arrange(agg_wide, desc(Diff_Percent))
agg <- filter(agg_wide, Diff_Percent > 0.15, is.na(StateName) == FALSE)
agg <- arrange(agg, order(Diff_Percent))
glimpse(agg)

#Exploratory Analysis - With Pipe
agg <- acc %>% 
  group_by(YEAR, StateName) %>% 
  summarize(TOTAL = sum(FATALS, na.rm = TRUE)) %>% 
  spread(YEAR, TOTAL) %>% 
  rename(Year2014 = "2014", Year2015 = "2015") %>% 
  mutate(Diff_Percent = (Year2015-Year2014)/Year2014) %>% 
  arrange(desc(Diff_Percent)) %>% 
  filter(Diff_Percent > 0.15, is.na(StateName) == FALSE) %>% 
  arrange(order(Diff_Percent))
glimpse(agg)