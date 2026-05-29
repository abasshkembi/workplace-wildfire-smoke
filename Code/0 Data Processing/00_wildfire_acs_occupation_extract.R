# Title: Extracting American Community Survey occupational data by census tract
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: Aug 28, 2025

# Description
# ----------- Extracting 5-year ACS data (end years 2010 -> 2021)
# ----------- By census tract
# ----------- By major SOC codes
# ----------- From `tigris` package


setwd("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Exposure Lab (local)/Projects/PhD work/Analysis/2 Workplace Wildfire Smoke/")

library(tidyverse)
library(tidycensus)

# decide to use post-2008 because of occupational differences in 2007 (likely due to 2010 SOC changes)
acs_vars_2008 <- load_variables(2010, dataset = "acs5", cache = TRUE)
acs_vars_2009 <- load_variables(2011, dataset = "acs5", cache = TRUE)
acs_vars_2010 <- load_variables(2012, dataset = "acs5", cache = TRUE)
acs_vars_2011 <- load_variables(2013, dataset = "acs5", cache = TRUE)
acs_vars_2012 <- load_variables(2014, dataset = "acs5", cache = TRUE)
acs_vars_2013 <- load_variables(2015, dataset = "acs5", cache = TRUE)
acs_vars_2014 <- load_variables(2016, dataset = "acs5", cache = TRUE)
acs_vars_2015 <- load_variables(2017, dataset = "acs5", cache = TRUE)
acs_vars_2016 <- load_variables(2018, dataset = "acs5", cache = TRUE)
acs_vars_2017 <- load_variables(2019, dataset = "acs5", cache = TRUE)
acs_vars_2018 <- load_variables(2020, dataset = "acs5", cache = TRUE)
acs_vars_2019 <- load_variables(2021, dataset = "acs5", cache = TRUE)

unique_state_fips <- state_laea$GEOID %>% sort()
SOC_ACS_crosswalk_2007 <- readxl::read_xlsx("Data/1 Input Data/ACS/SOC_ACS_crosswalk.xlsx", sheet = "2007")
unique_ACS_codes_2007 <- SOC_ACS_crosswalk_2007$ACS_code %>% unique()

# for years 2007
acs_years <- c(2009)

for(j in 1:length(acs_years)) {
  
  acs_year_i <- acs_years[j]
  
  real_year <- acs_year_i - 2
  full_acs_year <- NULL
  for(i in 1:length(unique_state_fips)) {
    temp_acs <- get_acs(
      geography = "county",
      variables = unique_ACS_codes_2007,
      year = acs_year_i,
      state = unique_state_fips[i]
    )
    full_acs_year <- rbind(full_acs_year, temp_acs)
  }
  saveRDS(full_acs_year, paste0("Data/1 Input Data/ACS/acs_occ_county_", real_year, ".rds"))
  print(acs_year_i)
  
}


SOC_ACS_crosswalk_2008_2019 <- readxl::read_xlsx("Data/1 Input Data/ACS/SOC_ACS_crosswalk.xlsx", sheet = "2008-2019")
unique_ACS_codes_2008_2019 <- SOC_ACS_crosswalk_2008_2019$ACS_code %>% unique()

# for years 2008 - 2019
acs_years <- c(2010:2021)

for(j in 1:length(acs_years)) {
  
  acs_year_i <- acs_years[j]
  
  real_year <- acs_year_i - 2
  full_acs_year <- NULL
  for(i in 1:length(unique_state_fips)) {
    temp_acs <- get_acs(
      geography = "county",
      variables = unique_ACS_codes_2008_2019,
      year = acs_year_i,
      state = unique_state_fips[i]
    )
    full_acs_year <- rbind(full_acs_year, temp_acs)
  }
  saveRDS(full_acs_year, paste0("Data/1 Input Data/ACS/acs_occ_county_", real_year, ".rds"))
  print(acs_year_i)
  
}





