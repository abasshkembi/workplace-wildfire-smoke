# Title: Estimate proportion of workers exposed to wildfire smoke
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: Aug 28, 2025

# Description
# ----------- generate daily estimates of the proportion of workers
# ----------- potentially exposed to wildfire PM2.5 >= 9 ug/m3
# ----------- for every county in the contiguous United States

setwd("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Exposure Lab (local)/Projects/PhD work/Analysis/2 Workplace Wildfire Smoke/")


library(tidyverse)
library(readxl)
library(lubridate)

# 1. read in primary datafiles

### daily, county, wildfire data - 2000-2020
wildfire <- readRDS("Data/1 Input Data/Wildfire/smokePM2pt5_predictions_daily_county_20060101-20201231.Rds") %>%
  rename(Date = date)
# we only need days >=9 ug/m3, and between 2006-2019
wildfire <- wildfire %>% filter(smokePM_pred >= 9) %>% filter(Date >= "2006-01-01") %>% filter(Date <= "2019-12-31")
wildfire <- wildfire %>%
  mutate(weekend_date = wday(Date, week_start = 1)) %>%
  mutate(weekend_date = ifelse(weekend_date > 5, 1, 0))
wildfire <- wildfire %>%
  mutate(ecc = case_when(
    smokePM_pred >= 9 & smokePM_pred < 35.5 ~ 2,
    smokePM_pred >= 35.5 & smokePM_pred < 55.5 ~ 3,
    smokePM_pred >= 55.5 & smokePM_pred < 125.5 ~ 4,
    smokePM_pred >= 125.5 & smokePM_pred < 225.5 ~ 5,
    smokePM_pred >= 225.5 ~ 6
  ))

smoke_days_2006_2019 <- wildfire$Date %>% unique()
length(smoke_days_2006_2019)
# 2261 days of data


### prepare a matrix of total employment by major SOC and county
county_emp <- readRDS("Data/2 Processed Data/BLS OEWS/process_oews_occ_county_2019.rds")
prep_exp <- county_emp %>%
  select(-soc_2010_title) %>%
  mutate(soc_2010_code = str_replace(soc_2010_code, "\\d{4}$", "0000")) %>%
  group_by(GEOID, major_soc = soc_2010_code) %>%
  summarise(major_tot_emp = sum(tot_emp_sum)) %>%
  ungroup()




### read in onet JEM characteristics on wildfire smoke exposure
onet_wildfire <- readRDS("Data/2 Processed Data/ONET/process_onet_wildfire_2019.rds") %>%
  mutate(major_soc = str_replace(soc_2010_code, "\\d{4}$", "0000"))

# raw estimates of nationwide proportion of outdoor workers by major soc
county_emp %>%
  left_join(onet_wildfire, by = "soc_2010_code") %>%
  mutate(out = prop_days_outdoors*tot_emp_sum) %>%
  group_by(major_soc) %>%
  summarise(n = sum(tot_emp_sum),
            n_out = sum(out)) %>%
  ungroup() %>%
  mutate(prop = n_out/n) %>%
  arrange(-prop) %>% 
  print(n = 22)




#### Starting code for exposure assessment

# filter data by year
mc_year_vector <- c(2006:2019)


### begin for loop by year
#length(mc_year_vector)
for(j in 1:length(mc_year_vector)) {
  
  wildfire_exposures_year <- wildfire %>% 
    filter(Date >= paste0(mc_year_vector[j], "-01-01")) %>% 
    filter(Date <= paste0(mc_year_vector[j], "-12-31")) ########### -12-31
  
  days_wildfire_year <- smoke_days_2006_2019[str_detect(smoke_days_2006_2019, paste0("^", mc_year_vector[j]))]
  
  
  
  ### begin for loop by day within year
  ####length(days_smoke_year)
  
  final_df_year <- NULL
  total_time <- 0
  for(i in 1:length(days_wildfire_year)) {
    
    start.time <- Sys.time()
    
    exp_day <- wildfire_exposures_year %>% 
      filter(Date == days_wildfire_year[i])
    
    exp_day2 <- exp_day %>%
      left_join(county_emp %>% select(-soc_2010_title), by = c("GEOID")) %>%
      left_join(onet_wildfire, by = "soc_2010_code") %>%
      
      mutate(prop_days_outdoors = ifelse(weekend_date == 1 & weekend_bin == 0, 0, prop_days_outdoors)) %>%
      filter(prop_days_outdoors > 0) %>%
      mutate(outdoor_emp = tot_emp_sum*prop_days_outdoors)
    
    temp_df <- exp_day2 %>%
      group_by(GEOID, major_soc, ecc) %>%
      summarise(exp_wildfire = sum(outdoor_emp),
                .groups = "keep") %>%
      ungroup() %>%
      inner_join(prep_exp, by = c("GEOID", "major_soc")) %>%
      mutate(exp_wildfire = exp_wildfire/major_tot_emp) %>%
      mutate(Date = days_wildfire_year[i]) 
    
    final_df_year <- rbind(final_df_year, temp_df)
    
    end.time <- Sys.time()
    time.taken <- as.numeric(end.time - start.time)
    total_time <- total_time + time.taken
    if(i %% (i/20) == 0) print(paste0("Iteration of ", mc_year_vector[j], ": ", i, " of ", length(days_wildfire_year) , " - time elapsed: ", round(total_time/60, 1), " min"))
    
  }
  
  saveRDS(final_df_year, paste0("Data/3 Generated Data/County Proportions/daily_wildfire_prop_occ_county_", mc_year_vector[j], ".rds"))
  
}










