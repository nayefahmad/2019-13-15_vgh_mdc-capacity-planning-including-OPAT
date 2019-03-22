

#***************************************************
# VGH MDC - Distributions of treatment volumes and durations 
# 2019-03-21
# Nayef 

#***************************************************

library(tidyverse)
library(magrittr)
library(lubridate)

# rm(list = ls())

# 1. Read in data: ------------------------------
source(here::here("src", 
                  "2019-03-21_vgh_mdc_read-data.R"))


# 2. Distributions: ----------------------------

p1.volumes <- 
  df1.volumes %>% 
  ggplot(aes(x = treatment, 
             y = volume)) + 
  geom_boxplot() + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95")); p1.volumes




p2.durations <- 
  df2.durations %>% 
  ggplot(aes(x = treatment, 
             y = duration)) + 
  geom_boxplot() + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95")); p2.durations
