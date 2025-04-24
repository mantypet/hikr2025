library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sf)
library(tmaptools)
library(httr)
library(jsonlite)

# Local data only! TODO: add accessible example data

# gpx_file <- here::here("data/ves2025.gpx")
kml_file <- here::here("data/ves2025.kml")

# ves_gpx <- tmaptools::read_GPX(gpx_file, remove.empty.layers = TRUE)[["route_points"]]

# Read KML points
sf::st_layers(kml_file)
ves <- st_read(kml_file, layer = "Points") |>
  transmute(id = Name, geometry)

coordinates <- sf::st_coordinates(ves) |>
  as_tibble()

points <- coordinates |>
  unite(location, Y,X, sep = ",", remove = TRUE) |>
  select(-Z) |>
  unlist()

# Get elevation from Google Maps Platform Elevation API
APIKEY <- rstudioapi::askForSecret("Google Maps Platform API key")

# Create API reader for call

points_text <- glue::glue_collapse(points, sep = "|")

outputFormat <- "json"
request <- glue::glue("https://maps.googleapis.com/maps/api/elevation/", outputFormat, "?")
key = glue::glue("key=", APIKEY)
locations <- glue::glue("locations=", points_text)

url <- glue::glue(request, locations, key, .sep = "&")

elevation <- httr::GET(url)
data <- fromJSON(rawToChar(elevation$content), simplifyDataFrame = TRUE) |>
  as_tibble() |>
  unnest(cols = c(results)) |>
  unnest(cols = c(location))

coordinates_ok <- data |>
  filter(status == "OK") |>
  transmute(X = lng,
            Y = lat,
            Z = elevation)

ves_elevation <- sf::st_as_sf(coordinates_ok, coords = c("X", "Y", "Z"), crs = 4326) |>
  mutate(id = ves$id) |>
  select(id, geometry)

# re-project to EUREF-FIN / TM35FIN(E,N) -- Finland
ves.rep <- ves_elevation |>
  sf::st_transform(3067) |>
  cbind(coordinates_ok)

plot(ves.rep)
plot(ves.rep$Z, type = "l")

break_proportional <- 0.1
min_km_moving <- 12
min_km_total <- 12/(1-break_proportional)

ves.rep.units <- ves.rep |>
  mutate(dist_m = as.numeric(st_distance(lag(geometry), geometry, by_element = TRUE))) |>
  mutate(dist_m = replace_na(dist_m, 0)) |>
  mutate(cum_dist_m = cumsum(dist_m)) |>
  mutate(t_min_avg_total = (cum_dist_m/1000)*min_km_total,
         t_h_avg_total = t_min_avg_total/60) |>
  mutate(Z_diff = replace_na((lag(Z)-Z),0),
         Z_gain = ifelse(Z_diff < 0, 0, Z_diff),
         Z_gain_cum = cumsum(Z_gain),
         grade = (Z_gain / dist_m)*100)

sum(ves.rep.units$dist_m)

plot(ves.rep.units["grade"], type = "b")


