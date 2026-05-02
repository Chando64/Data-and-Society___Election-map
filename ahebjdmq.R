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
map_va <- alarm_50state_map('VA')
va_cit_2023_cnty <- read_csv("va_cit_2023_cnty/va_cit_2023_cnty.csv")
va_inc_2023_cnty <- read_csv("va_inc_2023_cnty/va_inc_2023_cnty.csv")
va_race_2023_cnty <- read_csv("va_race_2023_cnty/va_race_2023_cnty.csv")
va_edu_2023_cnty <- read_csv("va_edu_2023_cnty/va_edu_2023_cnty.csv")
va_pov_2023_cnty <- read_csv("va_pov_2023_cnty/va_pov_2023_cnty.csv")

map_va$GEOID <- as.numeric(substr(as.character(map_va$GEOID), 1, 5))

va_race_inc <- inner_join(va_inc_2023_cnty, va_race_2023_cnty, 
                          by = c("COUNTYFP", "GEOID", "STATE", "COUNTY"))
va_pov_edu <- inner_join(va_edu_2023_cnty, va_pov_2023_cnty, 
                         by = c("COUNTYFP", "GEOID", "STATE", "COUNTY"))
va_all <- inner_join(va_race_inc, va_pov_edu, 
                     by = c("COUNTYFP", "GEOID", "STATE", "COUNTY"))

va_all_1 <- va_all |>
  group_by(COUNTY, GEOID) |>
  summarise(
    per_HS = (HS_DIP23/POP_25OV23)*100,
    per_noedu = (N_HSDIP23/POP_25OV23)*100 ,
    per_somcoll = (N_HSDIP23/POP_25OV23)*100 ,
    per_BA = (BACH_DEG23/POP_25OV23)*100 ,
    per_MA = (MAST_DEG23/POP_25OV23)*100 ,
    per_phd = (PROF_DEG23/POP_25OV23)*100,
    per_bpov = (TOT_BPOV23/TOT_HOUS23.x)*100 ,
    per_apov = (TOT_APOV23/ TOT_HOUS23.x)*100,
    per_mar = (TOT_MAR23/TOT_HOUS23.x)*100,
    tot_med = mean(MEDN_INC23),
    tot_mar = (sum(TOT_MAR23)/sum(TOT_HOUS23.x))*100,
    tot_chi = (sum(TOT_CHI23)/sum(TOT_HOUS23.x))*100
  ) |>
  ungroup()

va_all_2 <- va_all |>
  group_by(COUNTY, GEOID)|>
  summarise(inc_level = case_when(
    MEDN_INC23 >= 100000                      ~ "High",
    MEDN_INC23 > 69200 & MEDN_INC23 < 100000 ~ "Medium",
    MEDN_INC23 <= 69200                      ~ "Low",
    TRUE                                      ~ NA_character_))


va_all_4 <- inner_join(va_all_1, va_all_2, 
                     by = c("COUNTY", "GEOID"))

####Done in this order
map_va_23<- left_join(map_va, va_all_4, by = c("GEOID"))
map_va_23 <- map_va_23[,-c(8:40)]

map_va_23a <- map_va_23 |>
  group_by(cd_2020) |> 
  summarise(
    pp_scores = comp_polsby(plans = map_va_23$cd_2020, shp = map_va)
  ) |>
  ungroup()

####Compactness
bbox_scores <- comp_bbox_reock(plans = map_va_23$cd_2020, shp = map_va)
lw_scores<- comp_lw(plans = map_va_23$cd_2020, shp = map_va)
hul_scores <- comp_ch(plans = map_va_23$cd_2020, shp = map_va)

map_va_districts <- map_va |>
  group_by(cd_2020) |>
  summarise(geometry = sf::st_union(geometry), .groups = 'drop')

map_va_districts$bbox <- bbox_scores
map_va_districts$hul <- hul_scores
map_va_districts$lw <- lw_scores
map_va_districts$score <-((bbox_scores+hul_scores+lw_scores)/3)

map_va_districts_long <- map_va_districts |>
  pivot_longer(cols = c(bbox, hul, lw, score), 
               names_to = "metric", 
               values_to = "score")

ggplot(map_va_districts_long) +
  geom_sf(aes(fill = score), color = "white") +
  facet_wrap(~metric) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Virginia Congressional District Compactness",
       subtitle = "Four Different Metrics where high score is more compact 
while low score is less compact.",
       ) + 
  scale_fill_gradient(low = "yellow", high = "blue") +
  theme_void()


####bbox, hul, lw are best messures




