# Title: Processing ONET data for outdoor workers
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: Aug 28, 2025

# Description
# ----------- Get JEM of important variables from O*NET

setwd("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Exposure Lab (local)/Projects/PhD work/Analysis/2 Workplace Wildfire Smoke/")

library(tidyverse)

# prepare detailed soc codes we need for analysis
final_county_2019 <- readRDS("Data/2 Processed Data/BLS OEWS/process_oews_occ_county_2019.rds")

df_prep <- tibble(
  soc_2010_code = sort(unique(final_county_2019$soc_2010_code))
) %>%
  mutate(minor_2010 = str_replace(soc_2010_code, "\\d\\d$", "00"))





onet_context <- readxl::read_xlsx("Data/1 Input Data/ONET/db_24_1_excel/Work Context.xlsx")

# Element IDs we need
### 4.C.2.a.1.c - outdoors, exposed to weather
### 4.C.3.d.4 - regularity of work schedules
### 4.C.3.d.8 - typical work week duration

# preparing the dataset
onet_context2 <- onet_context %>%
  filter(`Scale Name` == "Context") %>% # for the final value out of 5 (multiply by 20)
  select(onet_soc = `O*NET-SOC Code`, 
         element_id = `Element ID`, 
         element_name = `Element Name`,
         scale_id = `Scale ID`, 
         value = `Data Value`) %>%
  filter(element_id %in% c("4.C.2.a.1.c", "4.C.3.d.4", "4.C.3.d.8")) %>%
  # normalize to 100
  mutate(value = 
           case_when(
             scale_id == "CX" ~ (value-1)*25, # scale to 100
             scale_id == "CT" ~ (value-1)*50 # scale to 100
           )) %>%
  mutate(
    element_var =
      case_when(
        element_id == "4.C.2.a.1.c" ~ "days_outdoors_100",
        element_id == "4.C.3.d.4" ~ "regularity_100",
        element_id == "4.C.3.d.8" ~ "weekduration_100"
      )
  ) %>%
  mutate(soc_2010_code = str_extract(onet_soc, "\\d\\d-\\d\\d\\d\\d")) %>%
  select(soc_2010_code, element_var, value) %>%
  group_by(soc_2010_code, element_var) %>%
  summarise(value = ceiling(mean(value, na.rm = TRUE))) %>%
  ungroup() %>%
  spread(element_var, value)

# link onet data to our soc data
df_prep2 <- df_prep %>% left_join(onet_context2, by = "soc_2010_code")

df_prep3 <- df_prep2 %>% 
  rowwise() %>% 
  mutate(nas = sum(is.na(across(matches("_100"))))) %>%
  ungroup()

df_prep3_complete <- df_prep3 %>% filter(nas == 0)
df_prep3_missing <- df_prep3 %>% filter(nas > 0)

df_prep3_minor <- df_prep3_complete %>% 
  select(-nas) %>%
  gather("element", "value", -c(soc_2010_code, minor_2010)) %>%
  group_by(minor_2010, element) %>%
  summarise(value = ceiling(mean(value, na.rm = TRUE))) %>%
  ungroup() %>%
  spread(element, value) %>%
  rowwise() %>% 
  mutate(nas = sum(is.na(across(matches("_100"))))) %>%
  ungroup()

df_prep3_missing_imputed <- df_prep3_missing %>%
  select(-c(days_outdoors_100:nas)) %>%
  left_join(df_prep3_minor, by = "minor_2010")



df_prep4 <- rbind(df_prep3_complete, df_prep3_missing_imputed)


# assign proportion of work-year outdoors

# creating crosswalk between context scores and equivalent workdays in a standard 250-day work-year
context2workdays <- tibble(Context = 0:100,
                           Days = c(ceiling(0:25/(25/(12/2))),
                                    ceiling(1:25/(25/((12+50)/2))+6),
                                    ceiling(1:25/(25/((50+150)/2))+37),
                                    ceiling(1:25/(25/(250-137))+137)))
# relationship visualized
context2workdays %>%
  ggplot(aes(x = Context, y = Days)) +
  geom_point(size = 0.5) +
  geom_point(data = tibble(Context = c(0, 25, 50, 75, 100), Days = c(0, 6, 37, 137, 250)),
             inherit.aes = F,
             aes(x = Context, y = Days), 
             color = "red", size = 2
  ) +
  ggrepel::geom_text_repel(
    data = tibble(Context = c(0, 25, 50, 75, 100), Days = c(0, 6, 37, 137, 250)) %>% mutate(label = paste(Days, "days")),
    inherit.aes = F,
    aes(x = Context, y = Days, label = label), 
    nudge_y = 20, nudge_x = -5, size = 4
  ) +
  theme_bw() +
  labs(x = "Context score (0-100)", y = "Days in work-year")


final_onet_wildfire <- df_prep4 %>%
  left_join(context2workdays, by = c("days_outdoors_100" = "Context")) %>%
  mutate(prop_days_outdoors = Days/250) %>% # proportion of work-year outdoors ### the final probability for MC simulation
  ## determine whether someone can be working on weekends
  mutate(irregular_bin = ifelse(regularity_100 > 25, 1, 0), # determine if irregular work
         gt40hrs_bin = ifelse(weekduration_100 > 50, 1, 0)) %>% # determine if working more than 40 hours
  mutate(weekend_bin = ifelse(irregular_bin + gt40hrs_bin > 0, 1, 0)) %>%
  select(soc_2010_code, weekend_bin, prop_days_outdoors)
  
summary(final_onet_wildfire)

saveRDS(final_onet_wildfire, "Data/2 Processed Data/ONET/process_onet_wildfire_2019.rds")


### some statistics on onet variable for supplement

final_onet_wildfire <- readRDS("Data/2 Processed Data/ONET/process_onet_wildfire_2019.rds")

# proportion of outdoor work
final_onet_wildfire %>%
  ggplot(aes(x = prop_days_outdoors)) +
  geom_histogram() +
  theme_classic() +
  labs(x = "Proportion of work-year outdoors",
       y = "Count of detailed SOC code")

# weekend work
final_onet_wildfire %>%
  ggplot(aes(x = weekend_bin)) +
  geom_bar() +
  theme_classic() +
  scale_x_continuous(breaks = c(0, 1)) +
  labs(x = "Work weekend",
       y = "Count of detailed SOC code")





