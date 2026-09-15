library(dplyr)
library(tidyverse)
library(readr)

# load data
zip_path <- "pa-flights.zip"

flights <- read_csv(unz(zip_path, "flights.csv"))
airlines <- read_csv(unz(zip_path, "airlines.csv"))
airports <- read_csv(unz(zip_path, "airports.csv"))
planes <- read_csv(unz(zip_path, "planes.csv"))
weather <- read_csv(unz(zip_path, "weather.csv"))

# Data Cleaning ----
sum(is.na(flights$dep_time)) # 3445 cancelled flights
sum(is.na(flights$arr_time)) # 3748 no arrival time
cancelled_flights <- flights %>% filter(is.na(dep_time))

cleaned_flights <- flights %>% filter(!is.na(dep_delay) & !is.na(arr_delay) & dep_delay >= 0) # depart delay
delay_summ <- cleaned_flights %>% group_by(origin, dest) %>% 
  summarise(
    delay_count = length(dep_delay),
    dep_delay_mean = mean(dep_delay),
    dep_delay_max = max(dep_delay),
    dep_delay_min = min(dep_delay),
    arr_delay_mean = mean(arr_delay),
    arr_delay_max = max(arr_delay),
    arr_delay_min = min(arr_delay)
    ) %>% 
  arrange(desc(dep_delay_mean))

# Select Rows ----
# PHL and SFO
cleaned_flights %>% filter(origin == "PHL" & dest == "SFO" & dep_delay > 120)

# Deriving New Measures ----
cleaned_flights <- cleaned_flights %>% mutate(red_eye = ifelse(hour >= 0 & hour <= 6, "Yes", "No"),
                           severity = ifelse(dep_delay == 0, "None",
                                             ifelse(dep_delay > 0 & dep_delay <= 60, "minor", "major")),
                           avg_grd_speed = distance/air_time)

# Grouping and Aggregation ----
carrier_delay <- cleaned_flights %>% group_by(carrier) %>% summarize(mean_delay = mean(dep_delay)) %>% arrange(desc(mean_delay))
carrier_delay

flights_planes <- cleaned_flights %>%
  left_join(planes %>% rename(manuf_year = year), by = "tailnum")
flights_planes %>% group_by(type, engine, speed, engines) %>% 
  summarise(avg_grd_speed = mean(avg_grd_speed),
            max_grd_speed = max(avg_grd_speed),
            min_grd_speed = min(avg_grd_speed))

PA_airports <- airports %>% 
  filter(tzone == "America/New_York") %>% 
  filter(lat >= 39.43, lat <= 42.16, lon >= -80.31, lon <= -74.41) 

PA_flights <- cleaned_flights %>% 
  left_join(PA_airports %>% rename(origin = faa), by = "origin") 
PA_flights %>% 
  group_by(month) %>% 
  summarize(tot_flights = n(), avg_dist = round(mean(distance), 2)) %>% as.data.frame()

# Joins ----
carrier_delay %>% left_join(airlines, by = "carrier") %>% select(name, mean_delay)

endpoints <- cleaned_flights %>% 
  left_join(airports %>% rename(dest = faa) %>% select(dest, name, tzone), by = "dest") %>% 
  rename(end_city = name)

# Pivot ----
cleaned_flights %>% group_by(carrier, month) %>% 
  summarize(avg_dep_delay = round(mean(dep_delay),2), .groups = "drop") %>% 
  mutate(month = month.abb[month]) %>% 
  pivot_wider(names_from = month, values_from = avg_dep_delay) %>% 
  as.data.frame()
