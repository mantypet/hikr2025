library(sf)

ves <- read_sf(here::here("data/ves2025.kml"))

# Create API reader for call
# https://maps.googleapis.com/maps/api/elevation/json?locations=LAT,LONG&key=YOURAPIKEY