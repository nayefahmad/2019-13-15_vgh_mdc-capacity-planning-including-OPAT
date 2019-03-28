

#***************************************************
# VGH MDC - PCA to identify dimensions of maximum variance
# 2019-03-27
# Nayef 

#***************************************************

# rm(list = ls())

library(broom)
library(ggfortify)

# 1. Read in data: ------------------------------
source(here::here("src", 
                  "2019-03-21_vgh_mdc_read-data.R"))


# > 1.1 reformat to wide again: -----------------

# todo: remove weekends

df2.1_dur_wide <- 
  df2.durations %>% 
  
  # add day of week: 
  mutate(weekday = weekdays(Date)) %>% 
  
  # remove weekends: 
  filter(!weekday %in% c("Saturday", "Sunday")) %>% 
  
  spread(key = treatment, 
         value = duration) %>% 
  select(-c(Date,
            weekday, 
            EDIV))  # drop EDIV bcoz it didn't actually occur in the MDC 





# 2. PCA biplot: ---------------

m1.pca <- prcomp(df2.1_dur_wide, 
                 center = TRUE, 
                 scale. = TRUE)


summary(m1.pca)

# Data is very spread out - We need 8 principal components to capture > 60% of
# the variance.

names(m1.pca)
m1.pca$rotation  # gives the loadings
m1.pca$x  # coordinates of every day with respect to the PCs

biplot(m1.pca, 
       main = "VGH MDC - Total hours by day \n(exluding weekends, and excluding EDIV)\nVariance explained: 19.7%\n", 
       sub = "\n\nEach point is a single day, \nprojected onto first 2 principal components")


pdf(here::here("results", 
               "dst", 
               "2019-03-27_vgh_mdc-pca-biplot.pdf"), 
    width = 10, 
    height = 10)
biplot(m1.pca, 
       main = "VGH MDC - Total hours by day \n(exluding weekends, and excluding EDIV)\nVariance explained: 19.7%\n", 
       sub = "\n\nEach point is a single day, \nprojected onto first 2 principal components")
dev.off()



# using ggfortify: 
autoplot(m1.pca, 
         loadings = TRUE, 
         loadings.labels = TRUE)
      


  
# 3. PCA loadings: -------------

names <- rownames(m1.pca$rotation)

df3.pca_loadings <- 
  m1.pca$rotation %>% 
  as.data.frame() %>% 
  select(1, 2) %>% 
  mutate(treatment = names) %>% 
  select(treatment, 
         everything())


summary(df3.pca_loadings)


df3.pca_loadings %>% arrange(desc(PC1))
df3.pca_loadings %>% arrange(desc(PC2))
