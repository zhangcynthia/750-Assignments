library(dplyr)
library(tidyverse)
library(readr)

# load data
zip_path <- "scripts/pa-flights.zip"

flights <- read_csv(unz(zip_path, "flights.csv"))
airlines <- read_csv(unz(zip_path, "airlines.csv"))
airports <- read_csv(unz(zip_path, "airports.csv"))
planes <- read_csv(unz(zip_path, "planes.csv"))
weather <- read_csv(unz(zip_path, "weather.csv"))

# Data Cleaning ----
sum(is.na(flights$dep_time)) # 3445 cancelled flights
sum(is.na(flights$arr_time)) # 3748 no arrival time
cancelled_flights <- flights %>% filter(is.na(dep_time))

# I dropped cancelled flights from the original data and saved it separately as a new dataframe in case for future use.

cleaned_flights <- flights %>% filter(!is.na(dep_delay) & !is.na(arr_delay) & dep_delay >= 0) # depart delay
delay_summ <- cleaned_flights %>% group_by(origin, dest) %>%
  summarise(
    delay_count = length(dep_delay),
    dep_delay_mean = mean(dep_delay),
    dep_delay_max = max(dep_delay),
    dep_delay_min = min(dep_delay)
    ) %>%
  arrange(desc(dep_delay_mean))
delay_summ

# Select Rows ----
# PHL and SFO
cleaned_flights %>% filter(origin == "PHL" & dest == "SFO" & dep_delay > 120)

# Deriving New Measures ----
cleaned_flights <- cleaned_flights %>% mutate(
  red_eye = ifelse(hour >= 0 & hour <= 6, "Yes", "No"),
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
  inner_join(PA_airports %>% rename(origin = faa), by = "origin")
PA_flights %>%
  group_by(month) %>%
  summarize(tot_flights = n(), avg_dist = round(mean(distance), 2)) %>%
  as.data.frame()

# Joins ----
carrier_delay %>% left_join(airlines, by = "carrier") %>% select(name, mean_delay)

airports_city <- airports %>% mutate(city = word(name, 1))
endpoints <- cleaned_flights %>%
  left_join(airports_city %>% rename(dest = faa) %>% select(dest, city, tzone), by = "dest") %>%
  rename(end_city = city)

# Pivot ----
cleaned_flights %>% group_by(carrier, month) %>%
  summarize(avg_dep_delay = round(mean(dep_delay),2), .groups = "drop") %>%
  mutate(month = month.abb[month]) %>%
  pivot_wider(names_from = month, values_from = avg_dep_delay) %>%
  as.data.frame()

# Answer Question ----
# Question: Among the top 5 busiest PA routes, which carrier has the most reliable arrival delays?
top_routes <- PA_flights %>%
  group_by(origin, dest) %>%
  summarise(n_flights = n(), .groups = "drop") %>%
  arrange(desc(n_flights)) %>%
  slice_head(n = 5)

carrier_reliable <- cleaned_flights %>%
  semi_join(top_routes, by = c("origin", "dest")) %>%
  group_by(carrier) %>%
  summarise(
    n_flights = n(),
    mean_arr_delay = mean(arr_delay),
    sd_arr_delay = sd(arr_delay)
  ) %>%
  filter(n_flights >= 30) %>%
  left_join(airlines, by = "carrier") %>%
  select(name, n_flights, mean_arr_delay, sd_arr_delay) %>%
  arrange(sd_arr_delay) %>% as.data.frame()
carrier_reliable

# Interpretation:
# Among top 5 busiest PA routes, Southwest Airlines is the most reliable carrier, with both the lowest mean arrival delay (13.1 minutes) and the lowest standard deviation (44.5). In contrast, United and JetBlue have the highest delay variability (SD > 115), meaning their passengers face much less predictable arrival times.
