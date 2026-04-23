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

fl_edu_2023_bg <- read_csv("fl_edu_2023_bg/fl_edu_2023_bg.csv")
fl_inc_2023_bg <- read_csv("fl_inc_2023_bg/fl_inc_2023_bg.csv")
fl_pov_2023_bg <- read_csv("fl_pov_2023_bg/fl_pov_2023_bg.csv")


fl_pov_edu <- inner_join(fl_edu_2023_bg, fl_pov_2023_bg, 
                         by = c("COUNTYFP", "GEOID", "STATE", "COUNTY"))
fl_all <- inner_join(fl_inc_2023_bg, fl_pov_edu, 
                     by = c("COUNTYFP", "GEOID", "STATE", "COUNTY"))

fl_all_1 <- fl_all |>
  group_by(COUNTY) |>
  filter(!is.na(COUNTY))|>
  reframe(
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
    tot_mar = mean(TOT_MAR23),
    tot_chi = mean(TOT_CHI23)
  )

fl_all_2 <- fl_all |>
  group_by(COUNTY)|>
  reframe(inc_level = case_when(
    MEDN_INC23 >= 115000                      ~ "High",
    MEDN_INC23 > 50000 & MEDN_INC23 < 115000 ~ "Medium",
    MEDN_INC23 <= 50000                       ~ "Low",
    TRUE                                      ~ NA_character_))


fl_all_4 <- inner_join(fl_all_1, fl_all_2, 
                       by = c("COUNTY"))

fl_all_5$inc_level <- factor(fl_all_5$inc_level,
                             levels = c("Low", "Medium", "High"),
                             ordered = TRUE)

g <- ggplot(data = fl_all_1, mapping = aes(x = tot_med, y = per_BA))
g + geom_jitter(aes(
  color = per_bpov, size = tot_chi), width = 0.2, height = 0, alpha = 0.8) +
  scale_color_gradient(low = "yellow", high = "purple") +
  labs(
    title = "Relationship between household income and 
  education level between counties in Virginia",
    subtitle = "Data used from the Data Hub",
    y = "Percentage of people 25yr+ with\na bachelor's degree (in percentages)",
    x = "Median household income (in dollars)",
    color = "Percent below\npoverty line",
    size = "Number of households 
with related children"
  ) +
  theme_minimal()+ theme(plot.background = element_rect(fill = "blue"))


fl_all_2 <- fl_all |>
  group_by(COUNTY)|>
  summarise(inc_level = case_when(
    MEDN_INC23 >= 100000                      ~ "High",
    MEDN_INC23 > 50000 & MEDN_INC23 < 100000 ~ "Medium",
    MEDN_INC23 <= 50000                       ~ "Low",
    TRUE                                      ~ NA_character_))
map_fl_2 <- map_fl |>
  group_by(county) |>
  summarize(
    pop_c = sum(pop)
  )
map_fl_22 <- inner_join( map_fl_2, fl_all_2,
                         by = c("county"="COUNTY" ))


ggplot(map_fl_22) +
  geom_sf(aes(fill=inc_level))  + 
  labs(title = "Virginia's Median Household Income ",
       subtitle = "The median household income level of each county where High is above $100,000, Median 
is between $100,000 to $50,000, and low is under $50,000",
       fill = "Income Level")+
  theme_void() 
