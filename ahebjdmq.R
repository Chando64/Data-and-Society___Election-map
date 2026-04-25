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
va_all_3 <- va_all_1 |>
  group_by(COUNTY, GEOID) |>
  summarise(edu_level =case_when(
    per_BA > 15 ~ "Above 15%",
    per_BA < 15 ~ "Below 15%"
  ))

va_all_4 <- inner_join(va_all_1, va_all_2, 
                     by = c("COUNTY", "GEOID"))
va_all_5 <- inner_join(va_all_4, va_all_3, 
                     by = c("COUNTY", "GEOID"))
####Done in this order
map_va_23<- left_join(map_va, va_all_5, by = c("GEOID"))
map_va_23 <- map_va_23[,-c(8:40)]

map_va_23a <- map_va_23 |> 
  filter(cd_2020 == 1)
map_va_23b <- map_va_23 |> 
  filter(cd_2020 == 2)
map_va_23c <- map_va_23 |> 
  filter(cd_2020 == 3)
map_va_23d <- map_va_23 |> 
  filter(cd_2020 == 4)
map_va_23e <- map_va_23 |> 
  filter(cd_2020 == 5)
map_va_23f <- map_va_23 |> 
  filter(cd_2020 == 6)
map_va_23g <- map_va_23 |> 
  filter(cd_2020 == 7)
map_va_23h <- map_va_23 |> 
  filter(cd_2020 == 8)


map_va_in_23a <- map_va_23 |>
  ggplot(aes(fill = per_bpov,ids = county, text = paste0(
               "County: ", county, "\n",
               "Socio-Economic Status\n",
               "Percentage graduate from some college: ", round(per_HS, 3)))) +
  geom_sf() +
  theme_void() +
  labs( title = "Virginia's Median Household Income
High > $100,000; Median $69,200–$100,000; Low < $69,200"
  ) 
  

ggplotly(map_va_in_1, tooltip = "text") |>
  style(hoveron = "fill")


