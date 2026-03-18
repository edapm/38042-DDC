rm(list=ls())
setwd("~/Documents/University/Geography Degree/02 Year 2/DDC/R/Final Project")

library(tidyverse)
library(lubridate)
library(tidytransit)
library(sf)

tfwm <- read_gtfs("tfwm.zip")

west_mids_metro <- tfwm$routes |>
  filter(agency_id == "OP7")

wmm_trips <- left_join(tfwm$trips, west_mids_metro, by="route_id") |>
  filter(agency_id == "OP7")

wmm_stoptimes <- left_join(tfwm$stop_times, wmm_trips, by="trip_id") |>
  filter(agency_id == "OP7")

wmm_stops <- left_join(tfwm$stops, wmm_stoptimes, by="stop_id") |>
  filter(agency_id == "OP7")

wmm_stops_time_sep <- wmm_stops |>
  separate(departure_time, sep = ":", into = c("dep_hours", "dep_mins", "dep_secs")) |>
  mutate_at(c("dep_hours", "dep_mins", "dep_secs"), as.numeric)

departures_nine_to_five <- wmm_stops_time_sep |>
  filter(dep_hours >= 9 & dep_hours <= 10) |> 
  filter(dep_mins >= 0 & dep_mins <= 1)

graph_data <- departures_nine_to_five |>
  mutate(departure_time = make_datetime(hour = dep_hours, min = dep_mins, sec = dep_secs))

ggplot(graph_data) + theme_bw() +
  geom_point(aes(y=trip_headsign, x=departure_time, color = trip_headsign), size = 0.2) +
  scale_x_time(breaks = seq(0, max(as.numeric(graph_data$departure_time)), 3600), 
               labels = scales::time_format("%H:%M")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Departures on WMM")
