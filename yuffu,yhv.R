library(alarmdata)
library(redist)
library(ggplot2)
library(plotly)
library(ggredist)
library(dplyr)
library(redistmetrics)
library(sf)
library(PL94171)
library(tinytiger)
library(tidyr)
library(dplyr)
library(alarmdata)
library(readr)
map_fl <- alarm_50state_map('FL')

####Compactness
bbox_scores <- comp_bbox_reock(plans = map_fl$cd_2020, shp = map_fl)
lw_scores<- comp_lw(plans = map_fl$cd_2020, shp = map_fl)
hul_scores <- comp_ch(plans = map_fl$cd_2020, shp = map_fl)

map_fl_districts <- map_fl |>
  group_by(cd_2020) |>
  summarise(geometry = sf::st_union(sf::st_make_valid(geometry)), .groups = 'drop')

map_fl_districts$bbox <- bbox_scores
map_fl_districts$hul <- hul_scores
map_fl_districts$lw <- lw_scores
map_fl_districts$score <-((bbox_scores+hul_scores+lw_scores)/3)

map_fl_districts_long <- map_fl_districts |>
  pivot_longer(cols = c(bbox, hul, lw, score), 
               names_to = "metric", 
               values_to = "score")

a <-ggplot(map_fl_districts_long) +
  geom_sf(aes(fill = score), color = "white") +
  facet_wrap(~metric) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Virginia Congressional District Compactness",
       subtitle = "Four Different Metrics where high score is more compact 
while low score is less compact.",
  ) + 
  scale_fill_gradient(low = "yellow", high = "blue") +
  theme_void()

plotly(a)


a <- ggplot(map_fl_districts_long) +
  geom_sf(aes(fill = score), color = "white") +
  facet_wrap(~metric) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Florida Congressional District Compactness",
       subtitle = "Four Different Metrics where high score is more compact 
while low score is less compact.") + 
  theme_void()

ggplotly(a)

