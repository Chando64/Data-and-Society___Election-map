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

# get the data for Virginia's, this may take a while
pl_raw_va <- pl_read(pl_url("VA", 2020))

# for redistricting we care about state-congressional districts so we group to 500 (if we want cds)
# precincts are 700 when we are looking at voting data
pl_va <- pl_subset(pl_raw_va, sumlev="700") |>
  pl_select_standard(clean_names=TRUE)

# load FL geometry
va_vds <- tt_voting_districts("Va")
names(va_vds) <- sub("(20)+$", "", names(va_vds))

pl_geo <- full_join(pl, va_vds, by="GEOID")

# remove the raw data as we no longer need it
rm(pl_raw_va)
