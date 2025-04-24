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

ves.rep <- ves_elevation |>
  sf::st_transform(3067)

plot(ves.rep)
