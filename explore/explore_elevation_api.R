library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sf)
library(tmaptools)

# Local data only! TODO: add accessible example data

# gpx_file <- here::here("data/ves2025.gpx")
kml_file <- here::here("data/ves2025.kml")

# ves_gpx <- tmaptools::read_GPX(gpx_file, remove.empty.layers = TRUE)[["route_points"]]

sf::st_layers(kml_file)
ves <- st_read(kml_file, layer = "Points") |>
  transmute(id = Name, geometry)


APIKEY <- rstudioapi::askForSecret("Google Maps Platform API key")

# Create API reader for call
# https://maps.googleapis.com/maps/api/elevation/json?locations=LAT,LONG&key=YOURAPIKEY


