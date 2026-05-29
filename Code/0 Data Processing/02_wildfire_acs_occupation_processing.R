# Title: Processing the ACS employment data
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: Aug 28, 2025

# Description
# ----------- Aggregating the ACS tract data to 2010 major SOC codes
# ----------- For 2018 and 2019, we first have to convert 2020 tract codes to 2010 tract codes

setwd("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Exposure Lab (local)/Projects/PhD work/Analysis/2 Workplace Wildfire Smoke/")

library(tidyverse)

SOC_ACS_crosswalk_2007 <- readxl::read_xlsx("Data/1 Input Data/ACS/SOC_ACS_crosswalk.xlsx", sheet = "2007")
SOC_ACS_crosswalk_2008_2019 <- readxl::read_xlsx("Data/1 Input Data/ACS/SOC_ACS_crosswalk.xlsx", sheet = "2008-2019")


acs_occ_2007 <- readRDS("Data/1 Input Data/ACS/acs_occ_county_2007.rds")
acs_occ_2007b <- acs_occ_2007 %>%
  left_join(SOC_ACS_crosswalk_2007, by = c("variable" = "ACS_code")) %>%
  group_by(GEOID, SOC_code, SOC_name) %>%
  summarise(estimate = sum(estimate),
            moe = sum(moe)) %>%
  ungroup()

saveRDS(acs_occ_2007b, paste0("Data/2 Processed Data/ACS/process_acs_occ_county_", 2007, ".rds"))





proc_years <- 2008:2019

for(i in 1:length(proc_years)) {
  #### i-th year
  temp_acs_occ <- readRDS(paste0("Data/1 Input Data/ACS/acs_occ_county_", proc_years[i], ".rds"))
  
  temp_df <- temp_acs_occ %>%
    left_join(SOC_ACS_crosswalk_2008_2019, by = c("variable" = "ACS_code")) %>%
    group_by(GEOID, SOC_code, SOC_name) %>%
    summarise(estimate = sum(estimate),
              moe = sum(moe)) %>%
    ungroup()
  
  saveRDS(temp_df, paste0("Data/2 Processed Data/ACS/process_acs_occ_county_", proc_years[i], ".rds"))
  print(proc_years[i])
}









