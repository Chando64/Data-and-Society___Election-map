library(alarmdata)
library(redist)
library(ggplot2)
library(plotly)
library(ggredist)
library(dplyr)
library(redistmetrics)
library(sf)
map_fl <- alarm_50state_map('FL')
plans_fl <- alarm_50state_plans('FL')
map_va <- alarm_50state_map('VA')
plans_va <- alarm_50state_plans('VA')

#hdjhhdfgah

map_va |> 
  ggplot() +
  geom_district(aes(group = cd_2020, fill = ndv, denom = nrv + ndv)) +
  scale_fill_party_c() +
  theme_map()

map_fl |> 
  st_make_valid() |>  #fixes the "hole to shell" issue
  ggplot() +
  geom_district(aes(group = cd_2020, fill = ndv, denom = nrv + ndv)) +
  scale_fill_party_c() +
  theme_map()
##########################################################
popu_va_1 <- map_va|>
  group_by(cd_2020) |>
  summarise(
    prop_hisp = (sum(pop_hisp)/sum(pop))*100,
    prop_white=(sum(pop_white)/sum(pop))*100,
    prop_black=(sum(pop_black)/sum(pop))*100,
    prop_aian=(sum(pop_aian)/sum(pop))*100,
    prop_asian=(sum(pop_asian)/sum(pop))*100,
    prop_nhpi=(sum(pop_nhpi)/sum(pop))*100,
    prop_other=(sum(pop_other)/sum(pop))*100,
    prop_two=(sum(pop_two)/sum(pop))*100,
    vote_share = sum(ndv) / (sum(ndv) + sum(nrv))
  )

popu_fl_1 <- map_fl|>
  st_make_valid() |>
  group_by(cd_2020) |>
  summarise(
    prop_hisp = (sum(pop_hisp)/sum(pop))*100,
    prop_white=(sum(pop_white)/sum(pop))*100,
    prop_black=(sum(pop_black)/sum(pop))*100,
    prop_aian=(sum(pop_aian)/sum(pop))*100,
    prop_asian=(sum(pop_asian)/sum(pop))*100,
    prop_nhpi=(sum(pop_nhpi)/sum(pop))*100,
    prop_other=(sum(pop_other)/sum(pop))*100,
    prop_two=(sum(pop_two)/sum(pop))*100,
    vote_share = sum(ndv) / (sum(ndv) + sum(nrv))
  )
###########################################################
map_va_in <- popu_va_1 |>
  ggplot(aes(fill = vote_share,text=paste(
    "District: ", cd_2020, "\n",
    "Hispanic population: ", round(prop_hisp, digits = 2),"%", "\n",
    "White Population: ", round(prop_white, digits = 2), "%", "\n",
    "Black Population: ", round(prop_black, digits = 2), "%", "\n",
    "Native Population: ", round(prop_aian, digits = 2), "%", "\n",
    "Asian Population: ", round(prop_asian, digits = 2), "%", "\n",
    "Islander Population: ", round(prop_nhpi, digits = 2), "%", "\n",
    "Other Population : ", round(prop_other, digits = 2), "%", "\n",
    sep=""))) +
  geom_sf(color = "gray", size = 2) +
  scale_fill_party_c() +
  theme_map()
ggplotly(map_va_in, tooltip = "text") |>
  style(hoveron = "fill") 


map_fl_in <- popu_fl_1 |>
  ggplot(aes(fill = vote_share,
             text=paste("District: ", cd_2020, "\n",
                        "Hispanic population: ", round(prop_hisp, digits = 2),"%", "\n",
                        "White Population: ", round(prop_white, digits = 2), "%", "\n",
                        "Black Population: ", round(prop_black, digits = 2), "%", "\n",
                        "Native Population: ", round(prop_aian, digits = 2), "%", "\n",
                        "Asian Population: ", round(prop_asian, digits = 2), "%", "\n",
                        "Islander Population: ", round(prop_nhpi, digits = 2), "%", "\n",
                        "Other Population : ", round(prop_other, digits = 2), "%", "\n",
                        sep=""))) +
  geom_sf(color = "gray", size = 2) +
  scale_fill_party_c() +
  theme_map()
#map_fl_in1 <- sf::st_coordinates(sf::st_cast(sf::st_geometry(map_fl_in), "MULTIPOLYGON"))#
ggplotly(map_fl_in, tooltip = "text")
