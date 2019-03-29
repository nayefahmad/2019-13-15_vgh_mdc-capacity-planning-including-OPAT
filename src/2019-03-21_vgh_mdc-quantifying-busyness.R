

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
  mutate(date_id = paste0("day_", 1:n())) %>% 
  
  # > todo: drop EDIV?? -----
  select(-EDIV)


# str(df2.1_dur_wide)


# 2. Create distance matrix: ----------
# first convert to matrix: 
m1.durations <- 
  df2.1_dur_wide %>% 
  select(-c(Date, date_id)) %>% 
  as.matrix()
  
rownames(m1.durations) <- df2.1_dur_wide$date_id

# m1.durations



# now find distances matrix:
d1.manhattan.distance <- 
  dist(m1.durations, 
       method = "manhattan")

# Manhattan dist is the same as summing hours across all treatments 

# Reference: https://link.springer.com/chapter/10.1007/3-540-44503-X_27 "the
# Manhattan distance metric L(1 norm) is consistently more preferable than the
# Euclidean distance metric L(2 norm) for high dimensional data mining
# applications"

# convert dist matrix back to dataframe: 
df3.durations.dist <- 
  d1.manhattan.distance %>% 
  as.matrix() %>% 
  as.data.frame()  
  




# 3. distances from origin: -------------
df4.dist_from_origin <- 
  df3.durations.dist %>% 
  select(day_181) %>% 
  rename(manhattan_dist_from_origin = day_181) %>% 
  
  mutate(metric = "total hours-all treatments") %>% 
  
  # get back actual dates: 
  mutate(date_id = rownames(m1.durations)) %>% 
  inner_join(df2.1_dur_wide) %>% 
  
  mutate(day_of_week = weekdays(Date)) %>% 
  
  select(date_id,
         Date,
         day_of_week,
         metric,
         manhattan_dist_from_origin, 
         everything()) %>%
  arrange(desc(manhattan_dist_from_origin))

# str(df4.dist_from_origin)
# summary(df4.dist_from_origin)

head(df4.dist_from_origin)

tail(df4.dist_from_origin %>% 
       filter(!day_of_week %in% c("Sunday", "Saturday")), 
     20)



# 4. Notes: ---------------

# Busiest days: 
# Friday, 2017-08-25, with distance 84 from origin
# Monday, 2017-09-11, with distance 84 from origin
# Wednesday, 2017-09-20, with distance 84 from origin

# Least busy weekday with nonzero volume: 
# Thursday, 2017-06-15, with distance 50 from origin 

# Weekdays with 0 treatments: stat hols? 

# The medoid day we identified was Thursday, 2017-09-13
# check: 
#   median(df4.dist_from_origin$manhattan_dist_from_origin)  # 65.5  
#   df4.dist_from_origin %>% filter(Date == "2017-09-13")    # 67.5 


# 5. plot total hours distributions: ---------------
df4.dist_from_origin %>% 
  filter(!day_of_week %in% c("Saturday", "Sunday")) %>%  
  ggplot(aes(x = manhattan_dist_from_origin)) + 
  geom_histogram(fill = "skyblue4", 
                 col = "black", 
                 binwidth = 5, 
                 boundary = 0, 
                 closed = "left") + 
  theme_light() + 
  scale_x_continuous(limits = c(-5,90), 
                     breaks = seq(0,90, 10)) + 
  labs(title = "Distribution of daily total treatment hours (excl. weekends, EDIV cases)", 
       subtitle = "Total hours measures treatment hours across all treatments for a single day \n10% of days have total hours > 80 hours", 
       caption = "\n\n Daily total treatment hours is equivalent to the Manhattan distance from origin") + 
  theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95"))
      

# save output
ggsave(here::here("results", 
                  "dst", 
                  "2019-03-29_total-hours-distribution.pdf"))

# cdf of Manhattan distances: 
df4.dist_from_origin %>% 
  filter(!day_of_week %in% c("Saturday", "Sunday")) %>%  
  ggplot(aes(x = manhattan_dist_from_origin)) + 
  stat_ecdf() + 
  
  geom_hline(yintercept = .90, 
             col = "firebrick") + 

  theme_light() + 
  coord_cartesian(xlim = c(40, 100)) + 
  labs(title = "Distribution of Manhattan distances from origin (excl weekends, EDIV cases)", 
       subtitle = "Distance measures cumulative total hours across all treatments for a single day") + 
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


# save output
ggsave(here::here("results", 
                  "dst", 
                  "2019-03-29_total-hours-ecdf.pdf"))



# 6. plot volumes distributions: ------------

# > 6.1 histogram: -----
df1.volumes %>% 
  group_by(Date) %>% 
  summarise(sum(volume)) %>% 
  rename(total_volume = `sum(volume)`) %>% 
  
  ggplot(aes(x = total_volume)) + 
  
  geom_histogram(fill = "skyblue4", 
                 col = "black", 
                 binwidth = 2, 
                 boundary = 0, 
                 closed = "left") + 
  
  geom_vline(xintercept = 32, 
             col = "firebrick") + 
  
  theme_light() + 
  scale_x_continuous(limits = c(0,40), 
                     breaks = seq(0,40, 2)) + 
  labs(title = "Distribution of daily total treatment volumes (excl. weekends)", 
       subtitle = "Typical day scenario (2017-07-06): 32 treatments \n\n90th percentile of daily volumes: 37 cases \n50th percentile: 29 cases") + 
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


# save output
ggsave(here::here("results", 
                  "dst", 
                  "2019-03-29_total-volume-distribution.pdf"))


# 6.2 ecdf: --------------------
df1.volumes %>% 
  group_by(Date) %>% 
  summarise(sum(volume)) %>% 
  rename(total_volume = `sum(volume)`) %>% 
  
  ggplot(aes(x = total_volume)) + 
  
  stat_ecdf() + 
  
  geom_hline(yintercept = 0.90, 
             col = "firebrick") + 
  
  theme_light() + 
  scale_x_continuous(limits = c(0,40), 
                     breaks = seq(0,40, 2)) + 
  labs(title = "Cumulative distribution of daily total treatment volumes (excl. weekends)", 
       subtitle = "10% of days have 37 or more treatments") + 
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))

# save output
ggsave(here::here("results", 
                  "dst", 
                  "2019-03-29_total-volume-ecdf.pdf"))



# 7. write outpus: ------------
write_csv(df3.durations.dist,
      here::here("results", 
             "dst", 
             "2019-03-21_distances-matrix.csv"))
             

write_csv(df4.dist_from_origin,
          here::here("results", 
                     "dst", 
                     "2019-03-21_distances-from-origin.csv"))


ggsave()