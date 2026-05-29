# Title: Poststratification of county exposure proportions
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: Aug 28, 2025

# Description
# ----------- poststratify exposure probabilities to employment counts
# ----------- to estimate number of workers potentially exposed
# ----------- by county year

setwd("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Exposure Lab (local)/Projects/PhD work/Analysis/2 Workplace Wildfire Smoke/")


library(tidyverse)
library(readxl)
library(lubridate)

wildfire <- readRDS("Data/1 Input Data/Wildfire/smokePM2pt5_predictions_daily_county_20060101-20201231.Rds") %>%
  rename(Date = date)

# data for poststratification
# employment counts by major SOC code, county, and year
years_vector <- 2006:2019
occ_county_year <- NULL
for(i in 1:length(years_vector)) {
  if(years_vector[i] < 2007) {
    temp_county_year <- readRDS(paste0("Data/2 Processed Data/ACS/process_acs_occ_county_", 2007, ".rds")) %>%
      select(GEOID, major_soc = SOC_code, estimate) %>%
      mutate(year = years_vector[i])
  } else if(years_vector[i] >= 2007) {
    temp_county_year <- readRDS(paste0("Data/2 Processed Data/ACS/process_acs_occ_county_", years_vector[i], ".rds")) %>%
      select(GEOID, major_soc = SOC_code, estimate) %>%
      mutate(year = years_vector[i])
  }
  
  occ_county_year <- rbind(occ_county_year, temp_county_year)
}

# county-level workforce estimates
county_workpop <- occ_county_year %>%
  group_by(GEOID, year) %>%
  summarise(workpop = sum(estimate)) %>%
  ungroup()

# read in daily, county exposure proportions
wildfire_work_year <- NULL
for(i in 2006:2019) {
  temp_read <- readRDS(paste0("Data/3 Generated Data/County Proportions/daily_wildfire_prop_occ_county_", i, ".rds"))
  wildfire_work_year <- rbind(wildfire_work_year, temp_read)
}

# calculate rate of workplace wildfire smoke exposure
# by county and year from 2006-2019
wildfire_county_rates <- wildfire_work_year %>%
  mutate(year = year(Date)) %>%
  left_join(occ_county_year, by = c("GEOID", "year", "major_soc")) %>%
  mutate(exp_wildfire = estimate*exp_wildfire) %>%
  group_by(GEOID, year) %>%
  summarise(exp_wildfire = sum(exp_wildfire, na.rm = T)) %>%
  ungroup() %>%
  full_join(occ_county_year %>% group_by(GEOID, year) %>% summarise(workpop = sum(estimate, na.rm = T)) %>% ungroup(), by = c("GEOID", "year")) %>%
  mutate(rate_wildfire_per10000 = exp_wildfire/(workpop*250)*10000,
         rate_wildfire_per10000 = ifelse(is.na(rate_wildfire_per10000), 0, rate_wildfire_per10000))

saveRDS(wildfire_county_rates, "Data/3 Generated Data/County Exposure Estimates/wildfire_exp_county_year_2006_2019.rds")

