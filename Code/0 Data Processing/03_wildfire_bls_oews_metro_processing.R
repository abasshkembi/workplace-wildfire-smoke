# Title: Processing BLS OEWS data
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: Aug 27, 2025

# Description
# ----------- Converting 2018 SOC codes to 2010 SOC codes
# ----------- Crosswalk 2010 counties to 2018 metro/nonmetro areas
# ----------- Assuming same occupational composition of counties within a metro/nonmetro area

setwd("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Exposure Lab (local)/Projects/PhD work/Analysis/2 Workplace Wildfire Smoke/")

library(tidyverse)

# a crosswalk between 2010 counties and 2018 metro/nonmetro areas
load("Data/1 Input Data/BLS OEWS/county_mnm_cross_2019.RData") # loads county_mnm_crosswalk

# 2018+ OEWS data had changes to metropolitan divisions and nonmetropolitan areas
#### see: https://www.bls.gov/oes/notices/2019/areas_2018.htm, "Upcoming Changes to May 2018 OES Metropolitan and Nonmetropolitan Data"
metro_division_crosswalk <- readxl::read_xlsx("Data/1 Input Data/BLS OEWS/divisions_2018.xlsx") %>%
  rename("area_2018_code" = 1, "area_2018_name" = 2, "area_2017_code" = 3, "area_2017_name" = 4)
nonmetro_2018_crosswalk <- readxl::read_xlsx("Data/1 Input Data/BLS OEWS/nonmet_2018.xlsx") %>%
  rename("area_2018_code" = 1, "area_2018_name" = 2, "area_2017_code" = 3, "area_2017_name" = 4)

nonmetro_division_2018_crosswalk <- rbind(metro_division_crosswalk, nonmetro_2018_crosswalk)

# 2004-2009 OEWS uses 2000 SOC
# 2010-2018 OEWS uses 2010 SOC
# 2019+ OEWS uses 2018 SOC
#### see: https://www.bls.gov/oes/oes_ques.htm, "Can OEWS data be used to look at changes in employment and wages over time?"
soc_2000_2010_crosswalk <- readxl::read_xls("Data/1 Input Data/BLS OEWS/soc_2000_to_2010_crosswalk.xls", skip = 6) %>%
  filter(!is.na(`2000 SOC code`)) %>%
  rename("soc_2000_code" = 1, "soc_2000_title" = 2, "soc_2010_code" = 3, "soc_2010_title" = 4) %>%
  group_by(soc_2000_code) %>%
  mutate(n_2000 = n()) %>%
  ungroup()
  
soc_2010_2018_crosswalk <- readxl::read_xlsx("Data/1 Input Data/BLS OEWS/soc_2010_to_2018_crosswalk.xlsx", skip = 7) %>%
  rename("soc_2010_code" = 1, "soc_2010_title" = 2, "soc_2018_code" = 3, "soc_2018_title" = 4) %>%
  group_by(soc_2018_code) %>%
  mutate(n_2018 = n()) %>%
  ungroup()




# 2019
mnm_2019_files <- list.files("Data/1 Input Data/BLS OEWS/2019/")
mnm_2019_files2 <- mnm_2019_files[!str_detect(mnm_2019_files, "field|file")]


final_mnm_2019 <- NULL
final_nrow <- NULL
for(i in 1:length(mnm_2019_files2)) {
  temp_mnm <- readxl::read_xlsx(paste0("Data/1 Input Data/BLS OEWS/2019/", mnm_2019_files2[i]))
  final_nrow <- c(final_nrow, nrow(temp_mnm))
  final_mnm_2019 <- rbind(final_mnm_2019, temp_mnm)
}
nrow(final_mnm_2019) == sum(final_nrow)


final_mnm_2019b <- final_mnm_2019 %>% 
  filter(!str_detect(occ_code, "-0000")) %>%
  select(area, area_title, occ_code, occ_title, tot_emp) %>%
  left_join(soc_2010_2018_crosswalk,
            by = c("occ_code" = "soc_2018_code")) %>%
  mutate(est_emp = as.numeric(tot_emp)/n_2018)


final_mnm_2019b %>% filter(is.na(soc_2010_code)) %>% count(occ_code)

final_mnm_2019b %>% nrow()

final_mnm_2019b_complete <- final_mnm_2019b %>% filter(!is.na(soc_2010_code))
final_mnm_2019b_missing <- final_mnm_2019b %>% filter(is.na(soc_2010_code))



soc_missing_cross_broad <- soc_2010_2018_crosswalk %>%
  filter(str_detect(soc_2010_title, ", All Other")) %>%
  select(soc_2010_code, soc_2010_title) %>%
  distinct() %>%
  mutate(missing_soc = str_extract(soc_2010_code, "\\d\\d-\\d\\d"))

soc_missing_cross_major <- soc_2010_2018_crosswalk %>%
  filter(str_detect(soc_2010_title, ", All Other")) %>%
  select(soc_2010_code, soc_2010_title) %>%
  distinct() %>%
  mutate(missing_soc = str_extract(soc_2010_code, "\\d\\d")) %>%
  group_by(missing_soc) %>%
  mutate(id = row_number()) %>%
  filter(id == max(id)) %>%
  select(-id) %>%
  ungroup()

final_mnm_2019b_missing2 <- final_mnm_2019b_missing %>%
  select(-c(soc_2010_code:est_emp)) %>%
  mutate(missing_soc = str_extract(occ_code, "\\d\\d-\\d\\d")) %>%
  left_join(soc_missing_cross_broad, by = "missing_soc")

### keep this one
final_mnm_2019b_missing2a <-  final_mnm_2019b_missing2 %>%
  filter(!is.na(soc_2010_code))

final_mnm_2019b_missing2b <-  final_mnm_2019b_missing2 %>%
  filter(is.na(soc_2010_code)) %>%
  select(-soc_2010_code, -soc_2010_title)


### keep this one
final_mnm_2019b_missing3 <- final_mnm_2019b_missing2b %>%
  mutate(missing_soc = str_extract(occ_code, "\\d\\d")) %>%
  left_join(soc_missing_cross_major, by = "missing_soc")

# should be true
nrow(final_mnm_2019b_missing) == (nrow(final_mnm_2019b_missing2a) + nrow(final_mnm_2019b_missing3))

# imputed missing 2010 soc codes
final_mnm_2019b_missing_imp <- rbind(final_mnm_2019b_missing2a, final_mnm_2019b_missing3)

### final dataset converting 2018 soc to 2010 soc
final_mnm_2019c <- final_mnm_2019b_complete %>%
  mutate(tot_emp = est_emp) %>%
  select(-soc_2018_title, -n_2018, -est_emp) %>%
  rbind(final_mnm_2019b_missing_imp %>% select(-missing_soc))








#### now need to convert mnm areas
final_mnm_2019c %>%
  left_join(nonmetro_division_2018_crosswalk, 
            by = c("area" = "area_2018_code")) %>%
  mutate(area_2017_code = ifelse(is.na(area_2018_name), area, area_2017_code),
         area_2017_name = ifelse(is.na(area_2018_name), area_title, area_2017_name))



# are there any MNM codes that don't appear between the county_mnm_cross dataframe and the OEWS data?
county_mnm_crosswalk %>% 
  group_by(MSA_code, MSA_name) %>%
  count() %>%
  ungroup() %>%
  left_join(
    final_mnm_2019c %>%
      group_by(area, area_title) %>%
      count() %>%
      ungroup(),
    by = c("MSA_code" = "area")
  ) %>%
  filter(is.na(n.y))
# no


final_county_2019 <- county_mnm_crosswalk %>%
  full_join(
    final_mnm_2019c,
    by = c("MSA_code" = "area")
  ) %>%
  group_by(GEOID, soc_2010_code, soc_2010_title) %>%
  summarise(tot_emp_sum = sum(as.numeric(tot_emp, na.rm = TRUE))) %>%
  ungroup() %>%
  filter(!is.na(tot_emp_sum))


saveRDS(final_county_2019, "Data/2 Processed Data/BLS OEWS/process_oews_occ_county_2019.rds")









