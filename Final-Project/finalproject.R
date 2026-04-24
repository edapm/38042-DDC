rm(list=ls())
setwd("~/Documents/University/Geography Degree/02 Year 2/DDC/R/Final-Project")

library(tidyverse)
library(lubridate)
library(tidytransit)
library(maptiles)
library(tidyterra)
library(sf)

tfwm <- read_gtfs("tfwm.zip")


gtfs <- set_servicepattern(tfwm)
gtfs <- gtfs_as_sf(gtfs)

gtfs$shapes$length <- st_length(gtfs$shapes)

shape_lengths <- gtfs$shapes |> 
  as.data.frame() |> 
  select(shape_id, length, -geometry)

service_pattern_summary <- gtfs$trips |>
  left_join(gtfs$.$servicepatterns, by="service_id") |> 
  left_join(shape_lengths, by="shape_id") |>
  left_join(gtfs$stop_times, by="trip_id") |> 
  group_by(servicepattern_id) |> 
  summarise(
    trips = n(), 
    routes = n_distinct(route_id),
    total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
    route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
    stops=(n_distinct(stop_id)/2))

service_pattern_summary <- gtfs$.$dates_servicepatterns |> 
  group_by(servicepattern_id) |> 
  summarise(days_in_service = n()) |> 
  left_join(service_pattern_summary, by="servicepattern_id")

service_ids <- gtfs$.$servicepatterns |>
  pull(service_id)

gtfs$trips |>
  filter(service_id %in% service_ids) |>
  group_by(service_id, route_id) |>
  left_join(gtfs$routes, by = "route_id") |>
  filter(agency_id == "OP100") |>
  summarise(count = n())

am_stop_freq <- get_stop_frequency(gtfs, start_time = 6*3600, end_time = 10*3600, 
                                   service_ids = service_ids, by_route = TRUE)

one_line_stops <- am_stop_freq |>
  left_join(gtfs$routes, by = "route_id") |>
  filter(agency_id == "OP100", direction_id == 0) |>
  left_join(gtfs$stops, by ="stop_id") |> 
  mutate(mean_headway_minutes = mean_headway/60)

one_line_stops_sf <- gtfs$stops |>
  right_join(one_line_stops, by="stop_id")

one_line_stops_sf_crs <- sf::st_transform(one_line_stops_sf, 4326)

basemap <- get_tiles(one_line_stops_sf_crs, provider="OpenStreetMap", zoom = 12)

one_line_stops_sf_crs |> 
  ggplot() + 
  geom_spatraster_rgb(data = basemap) +
  geom_sf(aes(color = mean_headway_minutes)) +
  labs(color = "Mean Headway (Mins)")

am_route_freq <- get_route_frequency(gtfs, service_ids = service_ids, 
                                     start_time = 6*3600, end_time = 10*3600)

routes_sf <- get_route_geometry(gtfs, service_ids = service_ids)
routes_sf <- routes_sf |> 
  inner_join(am_route_freq, by = "route_id")

routes_sf_crs <- sf::st_transform(routes_sf, 4326)

nxbus <- tfwm$routes |>
  filter(agency_id == "OP100")

routes_sf_crs <- routes_sf_crs |>
  inner_join(nxbus, by = "route_id")

routes_sf_crs |> 
  filter(median_headways < 10*60) |>
  ggplot() + 
  geom_sf(aes(colour=as.factor(median_headways))) + 
  labs(color = "Headways") +
  geom_sf_text(aes(label=route_short_name)) +
  theme_bw()

nxbus_trips <- left_join(tfwm$trips, nxbus, by="route_id") |>
  filter(agency_id == "OP100")

nxbus_stoptimes <- left_join(tfwm$stop_times, nxbus_trips, by="trip_id") |>
  filter(agency_id == "OP100")

nxbus_stops <- left_join(tfwm$stops, nxbus_stoptimes, by="stop_id") |>
  filter(agency_id == "OP100")

nxbus_stops_time_sep <- nxbus_stops |>
  separate(departure_time, sep = ":", into = c("dep_hours", "dep_mins", "dep_secs")) |>
  mutate_at(c("dep_hours", "dep_mins", "dep_secs"), as.numeric)

departures_nine_to_five <- nxbus_stops_time_sep |>
  filter(dep_hours >= 9 & dep_hours <= 16)

graph_data <- departures_nine_to_five |>
  mutate(departure_time = make_datetime(hour = dep_hours, min = dep_mins, sec = dep_secs)) |>
  filter(str_detect(route_short_name, "X"))

ggplot(graph_data) + theme_bw() +
  geom_point(aes(y=trip_headsign, x=departure_time, color = route_short_name), size = 0.2) +
  scale_x_time(breaks = seq(0, max(as.numeric(graph_data$departure_time)), 3600), 
               labels = scales::time_format("%H")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(override.aes = list(size = 3))) + 
  labs(x = "Departure Time", y = "Destination", color = "Route")
