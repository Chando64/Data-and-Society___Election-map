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


map_va |> 
  ggplot() +
  geom_district(aes(group = cd_2020, fill = adv_16, denom = arv_16 + adv_16)) +
  scale_fill_party_c() +
  theme_map()

map_va |> 
  ggplot() +
  geom_district(aes(group = cd_2020, fill = ndv, denom = ndv + nrv)) +
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
    prop_priz_16_cli = (sum(pre_16_dem_cli)/(sum(pre_16_dem_cli)+sum(pre_16_rep_tru)))*100,
    prop_priz_16_tru=(sum(pre_16_rep_tru)/(sum(pre_16_dem_cli)+sum(pre_16_rep_tru)))*100,
    prop_uss_18_kai=(sum(uss_18_rep_ste)/(sum(uss_18_dem_kai)+sum(uss_18_rep_ste)))*100,
    prop_uss_18_ste=(sum(uss_18_dem_kai)/(sum(uss_18_dem_kai)+sum(uss_18_rep_ste)))*100,
    prop_priz_20_bid=(sum(pre_20_dem_bid)/(sum(pre_20_dem_bid)+sum(pre_20_rep_tru)))*100,
    prop_priz_20_tru=(sum(pre_20_rep_tru)/(sum(pre_20_dem_bid)+sum(pre_20_rep_tru)))*100,
    prop_uss_20_war=(sum(uss_20_dem_war)/(sum(uss_20_dem_war)+sum(uss_20_rep_gad)))*100,
    prop_uss_20_ga=(sum(uss_20_rep_gad)/(sum(uss_20_dem_war)+sum(uss_20_rep_gad)))*100,
    vote_share = sum(ndv) / (sum(ndv) + sum(nrv))
  )

###########################################################
map_va_in <- popu_va_1 |>
  ggplot(aes(fill = vote_share,text=paste(
    "District: ", cd_2020, "\n",
    "Presidential Race\n",
    "Joe Biden(D) vote percentage: ", round(prop_priz_20_bid, digits = 2),"%", "\n",
    "Donald Trump(R) vote percentage: ", round(prop_priz_20_tru, digits = 2), "%", "\n",
    "Senator Race\n",
    "Mark Warner(D) vote percentage: ", round(prop_uss_20_war, digits = 2), "%", "\n",
    "Daniel Gade(R) vote percentage: ", round(prop_uss_20_ga, digits = 2), "%", "\n",
    sep=""))) +
  geom_sf(color = "gray", size = 2) +
  scale_fill_party_c() +
  theme_map() +
  labs(title="Election for the state of Virgina in 2020",
       subtitle= "Vote percentage of presidtial and sentor race") +
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

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
