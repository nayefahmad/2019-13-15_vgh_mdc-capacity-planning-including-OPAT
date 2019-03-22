

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
  scale_y_continuous(limits = c(0,25), 
                     breaks = seq(0, 25, 2)) + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95")); p1.volumes




p2.durations <- 
  df2.durations %>% 
  ggplot(aes(x = treatment, 
             y = duration)) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,60), 
                     breaks = seq(0, 60, 5)) +
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95")); p2.durations




# 3. IRON AND IVIG: ----------------
# > 3.1 Volumes -----

p3.iron_ivig_vol <- 
  df1.volumes %>% 
  filter(treatment %in% c("IRON", "IVIG")) %>% 
  spread(key = treatment, 
         value = volume) %>% 
  ggplot(aes(x = IRON, 
             y = IVIG)) + 
  geom_jitter(width = .1, 
              height = .1, 
              alpha = .4, 
              col = "skyblue4") +
  scale_x_discrete(limits = seq(0,13,1), 
                   breaks = seq(0,13,1), 
                   expand = c(0,0)) + 
  scale_y_discrete(limits = seq(0,12,1), 
                   breaks = seq(0,12,1), 
                   expand = c(0,0)) + 
  # coord_cartesian(xlim = c(0,13), 
  #                 ylim = c(0,11)) + 
  
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95")); p3.iron_ivig_vol
      

p4.iron_ivig_vol_2 <- 
  df1.volumes %>% 
  filter(treatment %in% c("IRON", "IVIG")) %>% 
  spread(key = treatment, 
         value = volume) %>% 
  ggplot(aes(x = IRON, 
             y = IVIG)) + 
  geom_bin2d(bins = 10) + 
  scale_x_discrete(limits = seq(0,13,1), 
                   breaks = seq(0,13,1), 
                   expand = c(0,0)) + 
  scale_y_discrete(limits = seq(0,12,1), 
                   breaks = seq(0,12,1), 
                   expand = c(0,0)) + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95")); p4.iron_ivig_vol_2



# > 3.2 durations: ------------
p5.iron_ivig_dur <- 
  df2.durations %>% 
  filter(treatment %in% c("IRON", "IVIG"), 
         duration > 0) %>% 
  spread(key = treatment, 
         value = duration) %>% 
  ggplot(aes(x = IRON, 
             y = IVIG)) + 
  geom_bin2d() + 
  # scale_x_discrete(limits = seq(0,13,1), 
  #                  breaks = seq(0,13,1), 
  #                  expand = c(0,0)) + 
  # scale_y_discrete(limits = seq(0,12,1), 
  #                  breaks = seq(0,12,1), 
  #                  expand = c(0,0)) + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95")); p5.iron_ivig_dur


pdf(here::here("results", 
               "dst", 
               "2019-03-21_vgh_mdc-iron-and-ivig-volumes-and-durations.pdf"), 
    width = 10)
p3.iron_ivig_vol
p4.iron_ivig_vol_2
p5.iron_ivig_dur
dev.off()
