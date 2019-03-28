

#***************************************************
# VGH MDC - PCA to identify dimensions of maximum variance
# 2019-03-27
# Nayef 

#***************************************************

# rm(list = ls())

# 1. Read in data: ------------------------------
source(here::here("src", 
                  "2019-03-21_vgh_mdc_read-data.R"))


# > 1.1 reformat to wide again: -----------------

# todo: remove weekends

df2.1_dur_wide <- 
  df2.durations %>% 
  spread(key = treatment, 
         value = duration) %>% 
  select(-c(Date, EDIV))





# 2. PCA: ---------------

m1.pca <- prcomp(df2.1_dur_wide, 
                 center = TRUE, 
                 scale. = TRUE)


summary(m1.pca)

# Data is very spread out - We need 7 principal components to capture > 60% of
# the variance.

names(m1.pca)
m1.pca$rotation

biplot(m1.pca)

  