

#***************************************************
# VGH MDC - Quantifying "busyness" of every day 
# 2019-03-21
# Nayef 

#***************************************************

# rm(list = ls())

# 1. Read in data: ------------------------------
source(here::here("src", 
                  "2019-03-21_vgh_mdc_read-data.R"))


# > 1.1 reformat to wide again: -----------------
df2.1_dur_wide <- 
  df2.durations %>% 
  spread(key = treatment, 
         value = duration) %>% 
  
  # add an empty row to represent the origin: 
  bind_rows(tibble::tribble(
                     ~Date, ~ALBU, ~BIRO, ~BIVG, ~BLDC, ~BMED, ~BRBC, ~CYTO, ~IRON, ~IVIG, ~LUMB, ~MEDS, ~PHLE, ~PICK, ~PLTI, ~RBCI, ~SHRT, ~SOLU, ~TEST, ~EDIV,
               "3/21/2019",     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0
               ) %>% 
              mutate(Date = mdy(Date))
            ) %>% 
  
  # add date_id: 
  mutate(date_id = paste0("day_", 1:n()))


# str(df2.1_dur_wide)


# 2. Create distance matrix: ----------
# first convert to matrix: 
m1.durations <- 
  df2.1_dur_wide %>% 
  select(-c(Date, date_id)) %>% 
  as.matrix()
  
rownames(m1.durations) <- df2.1_dur_wide$date_id

# m1.durations



# now find distances:
d1.euclid.distance <- 
  dist(m1.durations, 
       method = "euclidean")

# convert back to dataframe: 
df3.durations.dist <- 
  d1.euclid.distance %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  
  # we only care about distances from origin: 
  select(day_181) %>% 
  rename(euclid_dist_from_origin = day_181) %>% 
  
  # get back actual dates: 
  mutate(date_id = rownames(m1.durations)) %>% 
  inner_join(df2.1_dur_wide) %>% 
  
  select(1:3) %>% 
  arrange(desc(euclid_dist_from_origin))

head(df3.durations.dist)
str(df3.durations.dist)
