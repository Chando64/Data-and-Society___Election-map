library(alarmdata)
library(redist)
library(ggplot2)
library(plotly)
library(ggredist)
library(dplyr)
library(redistmetrics)
library(sf)
my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}
map_va <- alarm_50state_map('VA')
plans_va <- alarm_50state_plans('VA')

#### Map of vote count of districts
map_va_2 <- map_va |>
  group_by(cd_2020) |>
  summarize(
    vote_count = sum(adv_20)+sum(arv_20)
  )
ggplot(map_va_2) +
  geom_sf(aes(fill=vote_count)) +
  scale_fill_continuous(low="green", high = "black" , name = "Voters Population") + 
  labs(title = "Virginia's Voters population",
       subtitle = "Voting population of each district where dark 
indicates dense population and light green indicates high voting population")+
  theme_void() 

#### Map of vote count of precincts
map_va_3 <- map_va |>
  group_by(county) |>
  summarize(
    vote_count = sum(adv_20)+sum(arv_20)
  )
ggplot(map_va_3) +
  geom_sf(aes(fill=vote_count)) +
  scale_fill_continuous(low="green", high = "black" , name = "Vote Population") + 
  labs(title = "Virginia's Voters Population Across Districts",
       subtitle = "Voting population of each district where dark 
indicates dense population and light green indicates high voting population")+
  theme_void() 

####Map of precincts with interaction
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


map_fl |> 
  st_make_valid() |>  #fixes the "hole to shell" issue
  ggplot() +
  geom_district(aes(group = cd_2020)) +
  scale_fill_party_c() +
  theme_map()



