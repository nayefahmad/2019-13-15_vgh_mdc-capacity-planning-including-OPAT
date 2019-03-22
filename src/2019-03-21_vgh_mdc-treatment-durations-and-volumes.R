

#***************************************************
# VGH MDC - Distributions of treatment volumes and durations 
# 2019-03-21
# Nayef 

#***************************************************

library(tidyverse)
library(magrittr)
library(lubridate)


# 1. Read in data: ------------------------------
options(readr.default_locale=readr::locale(tz="America/Los_Angeles"))

# volumes: 
df1.volumes <- 
  read_csv(here::here("results",
                      "clean data", 
                      "2019-03-21_vgh_mdc-historical-treatment-volumes.csv"))  


df1.volumes %<>%    
  mutate(Date = mdy(Date))

str(df1.volumes)
summary(df1.volumes)


# durations: 
df2.durations <- 
  read_csv(here::here("results",
                      "clean data", 
                      "2019-03-21_vgh_mdc-historical-treatment-durations.csv"))  


df2.durations %<>%    
  mutate(Date = mdy(Date))

str(df2.durations)
summary(df2.durations)

