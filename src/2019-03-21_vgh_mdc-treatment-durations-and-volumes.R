

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
  labs(title = "VGH MDC: Distribution of daily volumes by treatment type", 
       subtitle = "3rd Apr 2017 to 29th Sep 2017") + 
  
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
  
  labs(title = "VGH MDC: Distribution of daily total hours by treatment type", 
       subtitle = "3rd Apr 2017 to 29th Sep 2017") + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95")); p2.durations



# save results: 
pdf(here::here("results", 
               "dst", 
               "2019-03-21_vgh_mdc-volume-and-duration-distributions.pdf"), 
    width = 10)
p1.volumes
p2.durations
dev.off()


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
  scale_y_discrete(limits = seq(0,11,1), 
                   breaks = seq(0,11,1), 
                   expand = c(0,0)) + 
  # coord_cartesian(xlim = c(0,13), 
  #                 ylim = c(0,11)) + 
  
  labs(title = "VGH MDC: Volumes of IRON and IVIG, by day", 
       subtitle = "3rd Apr 2017 to 29th Sep 2017") + 
  
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
  scale_y_discrete(limits = seq(0,11,1), 
                   breaks = seq(0,11,1), 
                   expand = c(0,0)) + 
  
  labs(title = "VGH MDC: Volumes of IRON and IVIG, by day (excluding weekends)", 
       subtitle = "3rd Apr 2017 to 29th Sep 2017") + 
  
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
  labs(title = "VGH MDC: Total hours of IRON and IVIG, by day (excluding weekends)", 
       subtitle = "3rd Apr 2017 to 29th Sep 2017") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95")); p5.iron_ivig_dur


# save results: 
pdf(here::here("results", 
               "dst", 
               "2019-03-21_vgh_mdc-iron-and-ivig-volumes-and-durations.pdf"), 
    width = 10)
p3.iron_ivig_vol
p4.iron_ivig_vol_2
p5.iron_ivig_dur
dev.off()
