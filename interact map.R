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

####Map of vote percentage of districts
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
       subtitle= "Vote percentage of presidtial and sentor race")

ggplotly(map_va_in, tooltip = "text") |>
  style(hoveron = "fill") 

###############################################################################
Rich_map <- map_va_22 |>
  ggplot(aes(fill = inc_level,
             text = paste0(county, " — Median classification: ", inc_level))) +
  geom_sf() +
  labs( title = "Virginia Median Household Income",
    subtitle = "High: > $100,000; Medium: $50,000–$100,000; Low: < $50,000",
    fill = "Income level") +
  theme_void()

ggplotly(Rich_map, tooltip = "text") |>
  style(hoveron = "fill")
