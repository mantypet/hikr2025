library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sf)
library(tmaptools)
library(httr)
library(jsonlite)
library(ggplot2)
library(slider)

# Local data only! TODO: add accessible example data

gpx_file <- here::here("data/2025-04-29-strava-400m.gpx")

pyrkka_gpx <- tmaptools::read_GPX(gpx_file, remove.empty.layers = TRUE)[["track_points"]] |>
  select(id = track_seg_point_id, time, ele, geometry)

pyrkka.rep <- pyrkka_gpx |>
  mutate(t_s = as.numeric(time-min(time, na.rm = T)),
         t = as.numeric(lead(time)-time),
         dist_m = as.numeric(st_distance(lag(geometry), geometry, by_element = TRUE)),
         dist_m.ma = slide_dbl(dist_m, ~mean(.x), .before = 6, .after = 6))

pyrkka.phase_manual <- pyrkka.rep |>
  mutate(phase = case_when(t_s >= 760 & t_s <= 858 ~ "m400|A|effort",
                           t_s >= 859 & t_s <= 1009 ~ "m400|A|rest",
                           t_s >= 1010 & t_s <= 1109 ~ "m400|B|effort",
                           t_s >= 1110 & t_s <= 1251 ~ "m400|B|rest",
                           t_s >= 1252 & t_s <= 1349 ~ "m400|C|effort",
                           t_s >= 1350 & t_s <= 1500 ~ "m400|C|rest",
                           t_s >= 1501 & t_s <= 1592 ~ "m400|D|effort",
                           t_s >= 1593 & t_s <= 1737 ~ "m400|D|rest",
                           t_s >= 1738 & t_s <= 1837 ~ "m400|E|effort"))
pyrkka.phase_manual <- pyrkka.phase_manual |>
  separate_wider_delim(col = phase, delim = "|", names = c("part", "set", "type"))

pyrkka.400 <- pyrkka.phase_manual |>
  filter(t_s >= 2110 & t_s <= 2142)

ggplot(pyrkka.400) +
  geom_line(aes(x = t_s, y = dist_m)) +
  geom_line(aes(x = t_s, y = dist_m.ma), color = "red") +
  scale_x_continuous(n.breaks = 40)

ggplot(pyrkka.rep) +
  geom_line(aes(x = t_s, y = dist_m)) +
  geom_line(aes(x = t_s, y = dist_m.ma), color = "red") +
  scale_x_continuous(n.breaks = 10)

pyrkka.m400 <- pyrkka.rep_ |>
  filter(part == "m400") |>
  mutate(i = 1) |>
  group_by(set, type) |>
  mutate(t_s_i = cumsum(i)-1)

pyrkka.m400 |>
  filter(type == "effort") |>
  ggplot() +
  geom_line(aes(x = t_s_i, y = dist_m.ma, group = set))
