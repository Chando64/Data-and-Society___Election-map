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
library(readr)

va_cit_2023_cnty <- read_csv("va_cit_2023_cnty/va_cit_2023_cnty.csv")
va_inc_2023_cnty <- read_csv("va_inc_2023_cnty/va_inc_2023_cnty.csv")
va_race_2023_cnty <- read_csv("va_race_2023_cnty/va_race_2023_cnty.csv")
va_edu_2023_cnty <- read_csv("va_edu_2023_cnty/va_edu_2023_cnty.csv")
va_pov_2023_cnty <- read_csv("va_pov_2023_cnty/va_pov_2023_cnty.csv")

va_race_inc <- inner_join(va_inc_2023_cnty, va_race_2023_cnty, 
                          by = c("COUNTYFP", "GEOID", "STATE", "COUNTY"))
va_pov_edu <- inner_join(va_edu_2023_cnty, va_pov_2023_cnty, 
                         by = c("COUNTYFP", "GEOID", "STATE", "COUNTY"))
va_all <- inner_join(va_race_inc, va_pov_edu, 
                     by = c("COUNTYFP", "GEOID", "STATE", "COUNTY"))

va_all_1 <- va_all |>
  group_by(COUNTYFP) |>
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
  )

va_all_2 <- va_all |>
  group_by(COUNTYFP)|>
  summarise(inc_level = case_when(
    MEDN_INC23 >= 115000                      ~ "High",
    MEDN_INC23 > 50000 & MEDN_INC23 < 115000 ~ "Medium",
    MEDN_INC23 <= 50000                       ~ "Low",
    TRUE                                      ~ NA_character_))
va_all_3 <- va_all_1 |>
  group_by(COUNTYFP) |>
  summarise(edu_level =case_when(
    per_BA > 15 ~ "Above 15%",
    per_BA < 15 ~ "Below 15%"
  ))

va_all_4 <- inner_join(va_all_1, va_all_2, 
                     by = c("COUNTYFP"))
va_all_5 <- inner_join(va_all_4, va_all_3, 
                     by = c("COUNTYFP"))

va_all_5$inc_level <- factor(va_all_5$inc_level,
                             levels = c("Low", "Medium", "High"),
                             ordered = TRUE)

g <- ggplot(data = va_all_5, mapping = aes(x = tot_med, y = per_BA))
g + geom_jitter(aes(
  color = per_bpov, size = tot_mar), width = 0.2, height = 0, alpha = 0.8) +
  scale_color_gradient(low = "gray", high = "purple") +
  labs(
title = "Relationship between household income and 
  education level between counties in Virginia",
    subtitle = "Data used from the Data Hub",
    y = "Percentage of people 25yr+ with\na bachelor's degree (in percentages)",
    x = "Median household income (in dollars)",
    color = "Percent of household below\npoverty line",
    size = "Percentage of couple households 
with related children"
  ) +
  theme_minimal()


va_all_2 <- va_all |>
  group_by(COUNTY)|>
  summarise(inc_level = case_when(
    MEDN_INC23 >= 100000                      ~ "High",
    MEDN_INC23 > 50000 & MEDN_INC23 < 100000 ~ "Medium",
    MEDN_INC23 <= 50000                       ~ "Low",
    TRUE                                      ~ NA_character_))
map_va_2 <- map_va |>
  group_by(county) |>
  summarize(
    pop_c = sum(pop)
  )
map_va_22 <- inner_join( map_va_2, va_all_2,
                         by = c("county"="COUNTY" ))
                        
  
ggplot(map_va_22) +
  geom_sf(aes(fill=inc_level))  + 
  labs(title = "Virginia's Median Household Income ",
       subtitle = "The median household income level of each county where High is above $100,000, Median 
is between $100,000 to $50,000, and low is under $50,000",
       fill = "Income Level")+
  theme_void() 





