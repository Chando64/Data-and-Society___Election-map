library(alarmdata)
library(redist)
library(ggplot2)
library(plotly)
library(ggredist)
library(dplyr)
library(redistmetrics)
library(sf)

map_va <- alarm_50state_map('VA')
plans_va <- alarm_50state_plans('VA')

map_va_2 <- map_va |>
  group_by(cd_2020) |>
  summarize(
    vote_share = sum(ndv)/(sum(ndv)+sum(nrv)),
    vote_count = sum(ndv)+sum(nrv)
  )


map_va_2 |>
  ggplot() +
  geom_district(aes(group = cd_2020, fill = Vote_count)) +
  geom_sf(color = "black") + 
  scale_fill_party_c() +
  theme_map()



map_va_1 <- map_va |>
  ggplot() +
  geom_sf(color = "gray", linewidth = 0.1, aes(group = cd_2020, fill = (ndv/(ndv+nrv)))) + 
  geom_district(aes(group = cd_2020, fill = ndv, denom = ndv + nrv)) +
  scale_fill_party_c() +
  theme_map()

ggplotly(map_va_1, tooltip = "text")



map_va |> 
  ggplot() +
  geom_district(aes(group = cd_2020, fill = ndv, denom = ndv + nrv)) +
  geom_sf(color = "gray", linewidth = 0.1, alpha = 0.3) + 
  scale_fill_party_c() +
  theme_map()

map_va |> 
  ggplot() +
  geom_district(aes(group = county), fill=(adv_20/(adv_20+arv_20))) +
  geom_sf(color = "gray", linewidth = 0.01, alpha = 0.3, alpha = 0.1) + 
  geom_sf(aes(group = cd_2020), color = "white", linewidth = 0.5, alpha = 0.1)+
  scale_fill_party_c() +
  theme_map()