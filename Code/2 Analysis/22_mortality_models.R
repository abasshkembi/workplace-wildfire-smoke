# Title: Mortality models
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: Apr 28, 2026

# Description
# ----------- examine relationship between ambient wildfire smoke,
# ----------- workplace wildfire smoke, and all-cause mortality rates

setwd("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Exposure Lab (local)/Projects/PhD work/Analysis/2 Workplace Wildfire Smoke/")

library(tigris)
library(sf)
library(MetBrewer)
library(tidyverse)
library(tidycensus)
library(readxl)
library(lubridate)
library(MetBrewer)
library(ggpubr)
library(fixest)

# get shapefile of all contiguous US counties
unique_state_fips <- state_laea %>%
  filter(!(GEOID %in% c("02", "15"))) %>%
  .$GEOID %>% unique() %>% sort()

us_counties <- NULL
for(i in 1:length(unique_state_fips)) {
  state_tracts <- counties(state = unique_state_fips[i], cb = T, year = 2017)
  
  us_counties <- rbind(us_counties, state_tracts)
}

us_states <- states(cb = T, year = 2017) %>%
  shift_geometry(geoid_column = "GEOID") %>%
  st_transform("EPSG:3082") %>%
  filter(STATEFP %in% unique_state_fips) %>%
  filter(!(STATEFP %in% c("02", "15")))





# read employment counts by major SOC group, county, and year
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

# total number of workers by county-year
county_workpop <- occ_county_year %>%
  group_by(GEOID, year) %>%
  summarise(workpop = sum(estimate)) %>%
  ungroup()


# read in workplace wildfire exposure rates
wildfire_exp_year <- readRDS("Data/3 Generated Data/County Exposure Estimates/wildfire_exp_county_year_2006_2019.rds")


# read in all-cause mortality rates for all ages

### 2010-2019
all_deaths_allages_yearly_2010_2019 <- readxl::read_xlsx("Data/1 Input Data/Mortality/Allcause/allcause_county_yearly_age0toInf_2010_2019.xlsx",
                                               col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric"),
                                               col_names = c("county_name", "GEOID", "year", "year_code", "deaths", "population"),
                                               skip = 1) %>%
  mutate(county_length = str_length(GEOID)) %>%
  mutate(GEOID = ifelse(county_length == 4, paste0("0", GEOID), GEOID)) %>%
  select(-county_length) %>%
  select(GEOID, year, all_deaths_allages = deaths, population)

### 2006-2009
all_deaths_allages_yearly_2006_2009 <- readxl::read_xlsx("Data/1 Input Data/Mortality/Allcause/allcause_county_yearly_age0toInf_2006_2009.xlsx",
                                                         col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric"),
                                                         col_names = c("county_name", "GEOID", "year", "year_code", "deaths", "population"),
                                                         skip = 1) %>%
  mutate(county_length = str_length(GEOID)) %>%
  mutate(GEOID = ifelse(county_length == 4, paste0("0", GEOID), GEOID)) %>%
  select(-county_length) %>%
  select(GEOID, year, all_deaths_allages = deaths, population)

### combine 2006-2019
all_deaths_allages_yearly_2006_2019 <- rbind(all_deaths_allages_yearly_2006_2009, all_deaths_allages_yearly_2010_2019)

### total number of deaths
all_deaths_allages_yearly_2006_2019$all_deaths_allages %>% sum(na.rm = T)

# all-cause deaths among the working age population (15-64 years old)

### 2010-2019
all_deaths_workpop_yearly_2010_2019 <- readxl::read_xlsx("Data/1 Input Data/Mortality/Allcause/allcause_county_yearly_age15to64_2010_2019.xlsx",
                                                         col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric"),
                                                         col_names = c("county_name", "GEOID", "year", "year_code", "deaths", "population"),
                                                         skip = 1) %>%
  mutate(county_length = str_length(GEOID)) %>%
  mutate(GEOID = ifelse(county_length == 4, paste0("0", GEOID), GEOID)) %>%
  select(-county_length) %>%
  select(GEOID, year, all_deaths_workpop = deaths, population_workpop = population)

### 2006-2009
all_deaths_workpop_yearly_2006_2009 <- readxl::read_xlsx("Data/1 Input Data/Mortality/Allcause/allcause_county_yearly_age15to64_2006_2009.xlsx",
                                                         col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric"),
                                                         col_names = c("county_name", "GEOID", "year", "year_code", "deaths", "population"),
                                                         skip = 1) %>%
  mutate(county_length = str_length(GEOID)) %>%
  mutate(GEOID = ifelse(county_length == 4, paste0("0", GEOID), GEOID)) %>%
  select(-county_length) %>%
  select(GEOID, year, all_deaths_workpop = deaths, population_workpop = population)

### combine 2006-2019
all_deaths_workpop_yearly_2006_2019 <- rbind(all_deaths_workpop_yearly_2006_2009, all_deaths_workpop_yearly_2010_2019)



### daily, county, wildfire data - 2006-2020
wildfire <- readRDS("Data/1 Input Data/Wildfire/smokePM2pt5_predictions_daily_county_20060101-20201231.Rds") %>%
  rename(Date = date)
### yearly ambient average by county
wildfire_year_2006_2019 <- wildfire %>%
  filter(Date >= "2006-01-01") %>% filter(Date <= "2019-12-31") %>%
  mutate(year = year(Date)) %>%
  group_by(GEOID, year) %>%
  summarise(mean_smokepm2.5 = sum(smokePM_pred)/365) %>%
  ungroup()

# read in model covariates

### daily, county, heat data - 2000-2020
heat <- readRDS("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Exposure Lab (local)/Projects/PhD work/Analysis/Aim 1 Occ Indicators/Data/Exposures/Heat/Heatvars_County_2000-2020_v1.2.Rds") %>% 
  filter(Date >= "2006-01-01") %>% filter(Date <= "2019-12-31")
heat <- heat %>% select(GEOID = StCoFIPS, Date, Tmean_C) %>% as_tibble()
heat_year <- heat %>% 
  mutate(year = year(Date), month = month(Date)) %>%
  mutate(summer = ifelse(month %in% c(5:9), 1, 0)) %>%
  group_by(GEOID, summer) %>%
  mutate(q90_temp = quantile(Tmean_C, probs = 0.9, na.rm = T)) %>%
  ungroup() %>%
  group_by(GEOID, year) %>%
  mutate(q90_temp_summer = q90_temp[which.max(summer)]) %>%
  ungroup() %>%
  mutate(above_q90_summer = ifelse(Tmean_C >= q90_temp_summer, 1, 0)) %>%
  group_by(GEOID, year) %>%
  summarise(mean_temp = mean(Tmean_C, na.rm = TRUE),
            sd_temp = sd(Tmean_C, na.rm = TRUE),
            n_q90 = sum(above_q90_summer, na.rm = T)) %>%
  ungroup()

### precipitation
precip_df <- NULL
for(i in 2006:2019) {
  temp_csv <- read.csv(paste0("Data/1 Input Data/Precipitation/noaa_precip_", i, ".csv"),
                       skip = 3) %>%
    as_tibble() %>%
    mutate(year = i) %>%
    select(ID, year, Value)
  
  precip_df <- rbind(precip_df, temp_csv)
}

precip_geoid_conv <- as_tibble(fips_codes) %>% 
  mutate(ID = paste0(state, "-", county_code),
         GEOID = paste0(state_code, county_code)) %>%
  select(ID, GEOID)

precip_final <- precip_df %>%
  left_join(precip_geoid_conv, by = "ID") %>%
  filter(!is.na(GEOID)) %>% ## drops GEOID 24511 in Maryland, which is not in our data anyways
  select(GEOID, year, precip_inches = Value)



# create final dataset for analysis

final_data_year <- expand_grid(
  GEOID = unique(occ_county_year$GEOID),
  year = 2006:2019
) %>%
  left_join(
    wildfire_exp_year, by = c("GEOID", "year")
  ) %>%
  left_join(all_deaths_allages_yearly_2006_2019, by = c("GEOID", "year")) %>%
  left_join(all_deaths_workpop_yearly_2006_2019, by = c("GEOID", "year")) %>%
  left_join(wildfire_year_2006_2019, by = c("GEOID", "year")) %>%
  left_join(heat_year, by = c("GEOID", "year")) %>%
  left_join(precip_final, by = c("GEOID", "year")) %>%
  mutate(mean_smokepm2.5 = ifelse(is.na(mean_smokepm2.5), 0, mean_smokepm2.5)) %>%
  mutate(exp_wildfire = ifelse(is.na(exp_wildfire), 0, exp_wildfire)) %>%
  mutate(rate_wildfire_per10000 = ifelse(is.na(rate_wildfire_per10000), 0, rate_wildfire_per10000)) %>%
  mutate(all_deaths_allages = ifelse(is.na(all_deaths_allages), 0, all_deaths_allages)) %>%
  mutate(all_deaths_workpop = ifelse(is.na(all_deaths_workpop), 0, all_deaths_workpop)) %>%
  mutate(precip_inches = ifelse(is.na(precip_inches), 0, precip_inches)) %>%
  mutate(state_fips = str_extract(GEOID, "^\\d\\d")) %>%
  mutate(state_year = paste0(state_fips, year)) %>%
  filter(!(state_fips %in% c("02", "15")))

# run some descriptives on the data

final_data_year$perc_outdoors %>% hist()

final_data_year$rate_wildfire_per10000 %>% summary()
final_data_year$perc_outdoors %>% summary()

final_data_year$mean_smokepm2.5 %>% density() %>% plot()
final_data_year$mean_smokepm2.5 %>% quantile(probs = seq(0, 1, by = 0.1))
final_data_year$rate_wildfire_per10000 %>% density() %>% plot()
final_data_year$rate_wildfire_per10000 %>% quantile(probs = seq(0, 1, by = 0.1))










########### ------------------------
########### Main models in main text
########### ------------------------


df_main <- final_data_year %>%
  # create categorical ambient variable as Qiu et al. did
  # and create trucated ambient variable
  mutate(mean_bins_qiu = cut(mean_smokepm2.5, breaks = c(-Inf, 0.1, 0.25, 0.5, 0.75, 1, 2, 3, 4, Inf)),
         mean_bins_abas = cut(mean_smokepm2.5,  breaks= c(-Inf, 0.1, 0.25, 0.5, 0.75, Inf))) %>%  
  # classify workplace exposures by no vs any workplace exposures
  mutate(rate_bin = case_when(
    rate_wildfire_per10000 <= 0 ~ "0",
    rate_wildfire_per10000 > 0 ~ "2"
  )) %>%
  # classify interaction between ambient and workplace exposures
  mutate(mean_rate_abas = interaction(mean_bins_abas, rate_bin),
         mean_rate_qiu = interaction(mean_bins_qiu, rate_bin))

### prepare dataset for models
df_allcause <- df_main %>%
  # impute missing deaths at 4.5 deaths
  mutate(all_deaths_allages = ifelse(all_deaths_allages < 10, 4.5, all_deaths_allages)) %>%
  mutate(all_deaths_workpop = ifelse(all_deaths_workpop < 10, 4.5, all_deaths_workpop)) %>%
  # complete cases for these variables
  na.omit(population, all_deaths_allages, mean_bins_abas, mean_bins_qiu, state_year, mean_temp, precip_inches, GEOID, population)







### final boot strap models
#### ambient only first
## bootstrap for SEs 500 times
ambient_allcause_fepois_df <- NULL
for(i in 1:500) {
  
  set.seed(123+i); temp_fepois_df <- df_allcause[sample(1:nrow(df_allcause), nrow(df_allcause), replace = T),]
  
  temp_fepois_model_ambient <- feglm(all_deaths_allages ~ relevel(mean_bins_abas, 1) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                data = temp_fepois_df,
                                family = quasipoisson,
                                warn = F,
                                notes = F)
  
  temp_coef_fepois_ambient <- as.vector(coef(temp_fepois_model_ambient)[1:4])
  
  temp_effects <- tibble(iter = i,
                         ambient = rep(c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75+"), 1),
                         beta = c(0, temp_coef_fepois_ambient)
  )
  
  ambient_allcause_fepois_df <- rbind(ambient_allcause_fepois_df, temp_effects)
  if(i%%50 == 0) print(i)
}

rr_ambient_allcause_fepois_df <- ambient_allcause_fepois_df %>%
  mutate(ambient = factor(ambient, levels = c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75+"))) %>%
  group_by(ambient) %>%
  summarise(rr = exp(median(beta))-1, 
            ll = exp(quantile(beta, probs = 0.025))-1, 
            ul = exp(quantile(beta, probs = 0.975))-1) %>%
  ungroup()

#### workplace and ambient next
## bootstrap for SEs 500 times
work_allcause_fepois_df <- NULL
for(i in 1:500) {
  
  set.seed(123+i); temp_fepois_df <- df_allcause[sample(1:nrow(df_allcause), nrow(df_allcause), replace = T),]
  
  temp_fepois_model_0 <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 1) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                data = temp_fepois_df,
                                family = quasipoisson,
                                warn = F,
                                notes = F)
  
  temp_fepois_model_1 <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 6) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                data = temp_fepois_df,
                                family = quasipoisson,
                                warn = F,
                                notes = F)
  
  temp_coef_fepois_0 <- as.vector(coef(temp_fepois_model_0)[1:4])
  temp_coef_fepois_1 <- as.vector(coef(temp_fepois_model_1)[6:9])
  
  temp_effects <- tibble(iter = i,
                         work = c(rep("0", 5), rep(">0", 5)),
                         ambient = rep(c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75+"), 2),
                         beta = c(0, temp_coef_fepois_0,
                                  0, temp_coef_fepois_1)
  )
  
  work_allcause_fepois_df <- rbind(work_allcause_fepois_df, temp_effects)
  if(i%%50 == 0) print(i)
}

rr_work_allcause_fepois_df <- work_allcause_fepois_df %>%
  mutate(work = factor(work, levels = c("0", ">0"),
                       labels = c("No work exposure", "Work exposure")),
         ambient = factor(ambient, levels = c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75+"))) %>%
  group_by(work, ambient) %>%
  summarise(rr = exp(median(beta))-1, 
            ll = exp(quantile(beta, probs = 0.025))-1, 
            ul = exp(quantile(beta, probs = 0.975))-1) %>%
  ungroup()

### exposure-response for ambient only model

fig2_b1 <- rr_ambient_allcause_fepois_df %>%
  mutate(title = "Not stratified by work") %>%
  ggplot(aes(x = ambient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  #geom_errorbar(aes(ymin = ll, ymax = ul, group = title, color = ambient), width = 0) +
  geom_ribbon(aes(ymin = ll, ymax = ul, group = title), color = "grey90", alpha = 0.2)+
  geom_line(aes(y = rr, group = title), color = "black") +
  geom_point(aes(y = rr, group = title), color = "black") +
  #geom_point(aes(y = rr, group = title, color = ambient)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) +
  scale_color_manual(values = c("grey60", rev(met.brewer("Greek")))) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.1),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.minor.ticks.y.left = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.minor.ticks.length = unit(0.07, "cm"),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = NULL,
       y = "Percent difference in\nall-cause mortality rate") +
  coord_cartesian(ylim = c(-0.032, 0.032)) +
  facet_wrap(~title, nrow = 1)

# for supplemental table of effects

df_allcause %>%
  count(mean_bins_abas)

rr_ambient_allcause_fepois_df %>%
  mutate(rr = round(rr, 3)*100,
         ll = round(ll, 3)*100,
         ul = round(ul, 3)*100)

df_allcause %>%
  count(mean_rate_abas)

rr_work_allcause_fepois_df %>%
  mutate(rr = round(rr, 3)*100,
         ll = round(ll, 3)*100,
         ul = round(ul, 3)*100)



### exposure-response modified by workplace exposure
fig2_b23 <- rr_work_allcause_fepois_df %>%
  ggplot(aes(x = ambient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  #geom_errorbar(aes(ymin = ll, ymax = ul, group = work, color = ambient), width = 0) +
  geom_ribbon(aes(ymin = ll, ymax = ul, group = work, fill = work), alpha = 0.2)+
  geom_line(aes(y = rr, group = work, color = work)) +
  geom_point(aes(y = rr, group = work, color = work)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) +
  scale_color_manual(values = c(met.brewer("Homer1")[c(4, 2)])) +
  scale_fill_manual(values = c(met.brewer("Homer1")[c(4, 2)])) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.1),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.ticks = element_line(color = "black"),
        axis.minor.ticks.y.left = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.minor.ticks.length = unit(0.07, "cm"),
        legend.position = "none") +
  labs(x = NULL,
       y = "Percent difference in\nall-cause mortality rate") +
  coord_cartesian(ylim = c(-0.032, 0.032)) +
  facet_wrap(~work, nrow = 1)

ggarrange(
  fig2_b1 + theme(plot.margin = unit(c(0, 5.5, 5.5, 5.5), 'points')), 
  fig2_b23 + theme(plot.margin = unit(c(0, 5.5, 5.5, 5.5), 'points')),
  nrow = 1, widths = c(0.33, 0.6)
)



## histogram of county-year counts
# ambient only
fig_hist_a <- df_allcause %>%
  mutate(ambient = factor(mean_bins_abas, labels = c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75+"))) %>%
  count(ambient) %>%
  ungroup() %>%
  mutate(title = "Not stratified by work") %>%
  mutate(perc = n/nrow(df_allcause)) %>%
  ggplot(aes(x = ambient)) +
  geom_col(aes(y = perc), fill = "grey60", width = 0.75) +
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3), labels = scales::percent) +
  #scale_fill_manual(values = c("grey60", rev(MetBrewer::met.brewer("Greek")))) +
  theme_classic() +
  theme(#panel.background = element_rect(color = "black", fill = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_blank(),
    axis.line = element_blank(),
    #strip.text = element_text(size = 11),
    axis.title = element_text(size = 10),
    axis.title.y = element_text(hjust = 1),
    axis.text.x = element_blank(),
    axis.ticks = element_line(color = "transparent"),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none") +
  labs(x = NULL,
       y = "County-\nyears") +
  coord_cartesian(ylim = c(-0.052, 0.3)) +
  facet_wrap(~title, nrow = 1)

# by workplace
fig_hist_b <- df_allcause %>%
  mutate(work = factor(rate_bin, labels = c("No work exposure", "Work exposure")),
         ambient = factor(mean_bins_abas, labels = c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75+"))) %>%
  count(work, ambient) %>%
  ungroup() %>%
  mutate(n = ifelse(n < 100, 200, n)) %>%
  mutate(perc = n/nrow(df_allcause)) %>%
  ggplot(aes(x = ambient)) +
  geom_col(aes(y = perc, group = work, fill = work), width = 0.75) +
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3), labels = scales::percent) +
  scale_fill_manual(values = c(met.brewer("Homer1")[c(4, 2)])) +
  theme_classic() +
  theme(#panel.background = element_rect(color = "black", fill = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_blank(),
    axis.line = element_blank(),
    #strip.text = element_text(size = 11),
    axis.title = element_text(size = 10),
    axis.title.y = element_text(hjust = 1),
    axis.text.x = element_blank(),
    axis.ticks = element_line(color = "transparent"),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none") +
  labs(x = NULL,
       y = "County-\nyears") +
  coord_cartesian(ylim = c(-0.052, 0.3)) +
  facet_wrap(~work, nrow = 1)

fig_2b <- ggarrange(
  ggarrange(
    fig_hist_a + theme(plot.margin = unit(c(5.5, 5.5, 0, 5.5), 'points')), 
    fig_hist_b + theme(plot.margin = unit(c(5.5, 5.5, 0, 5.5), 'points')),
    nrow = 1, widths = c(0.33, 0.6)
  ),
  NULL,
  ggarrange(
    fig2_b1 + theme(plot.margin = unit(c(0, 5.5, 5.5, 5.5), 'points')), 
    fig2_b23 + theme(plot.margin = unit(c(0, 5.5, 5.5, 5.5), 'points')),
    nrow = 1, widths = c(0.33, 0.6)
  ),
  nrow = 3, heights = c(0.3, -0.055, 0.7), align = "hv"
)

fig_2b




ggarrange(
  annotate_figure(
    fig_2a + labs(title = NULL),
    top = text_grob("", 
                    size = 13, just = 0.5)
    ),
  annotate_figure(fig_2b,
                  top = text_grob("", 
                                  size = 13, 0.5),
                  bottom = text_grob(expression(paste("Average county-year wildfire smoke ", PM[2.5], " ", ("µg" / m^3))),
                                     size = 10.5)),
  nrow = 2,
  heights = c(0.4, 0.6),
  labels = "auto"
  )















# sensitivity analysis

##### 1. using qiu model specifications

### compare to qiu et al

allcause_fepois_model_qiu <- feglm(all_deaths_allages ~ mean_bins_qiu + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                   data = df_allcause,
                                   family = quasipoisson,
                                   warn = F,
                                   notes = F)

### compare to truncated

allcause_fepois_model_trunc <- feglm(all_deaths_allages ~ mean_bins_abas + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                   data = df_allcause,
                                   family = quasipoisson,
                                   warn = F,
                                   notes = F)

sens_df_1 <- allcause_fepois_model_qiu$coeftable %>%
  mutate(var = rownames(.)) %>%
  as_tibble() %>%
  select(var, beta = Estimate, se = `Std. Error`) %>%
  filter(str_detect(var, "mean_bins_qiu")) %>%
  mutate(var = str_remove(var, "mean_bins_qiu")) %>%
  rbind(tibble(var = "(-Inf,0.1]", beta = 0, se = 0)) %>%
  mutate(model = "Replicated model") %>%
  rbind(
    allcause_fepois_model_trunc$coeftable %>%
      mutate(var = rownames(.)) %>%
      as_tibble() %>%
      select(var, beta = Estimate, se = `Std. Error`) %>%
      filter(str_detect(var, "mean_bins_abas")) %>%
      mutate(var = str_remove(var, "mean_bins_abas")) %>%
      rbind(tibble(var = "(-Inf,0.1]", beta = 0, se = 0)) %>%
      mutate(model = "Truncated model") %>%
      mutate(var = ifelse(var == "(0.75, Inf]", "(0.75,1]", var))
  ) %>%
  mutate(rr = exp(beta)-1, ll = exp(beta-3*se)-1, ul = exp(beta+3*se)-1) %>%
  select(-beta, -se) %>%
  rbind(
    tibble(
      var = c("(-Inf,0.1]", "(0.1,0.25]", "(0.25,0.5]", "(0.5,0.75]", "(0.75,1]", "(1,2]", "(2,3]", "(3,4]", "(4, Inf]"),
      model = c(rep("Qiu et al.", 9)),
      rr = c(0, 0.653, 0.914, 1.183, 1.462, 0.960, 0.685, 0.904, 3.899)/100,
      ll = c(0, 0.3344, 0.4989, 0.6364, 0.7850, 0.2001, -0.8352, -1.4256, 2.6099)/100,
      ul = c(0, 0.9715, 1.3288, 1.7302, 2.1382, 1.7209, 2.2060, 3.2345, 5.1880)/100
    )
  ) %>%
  mutate(ambient = factor(var, 
                          levels = c("(-Inf,0.1]", "(0.1,0.25]", "(0.25,0.5]", "(0.5,0.75]", "(0.75,1]", "(1,2]", "(2,3]", "(3,4]", "(4, Inf]"),
                          labels = c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75-1", "1-2", "2-3", "3-4", "4+")))

sens_df_1 %>%
  #mutate(beta = beta+0.015) %>%
  ggplot(aes(x = ambient, group = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_errorbar(aes(ymin = ll, ymax = ul), width = 0, color = "black") +
  geom_point(aes(y = rr,), color = "black", position=position_dodge(width=0.5), size = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = scales::percent) +
  theme_classic() +
  theme(panel.background = element_rect(color = "black", fill = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = NULL,
       y = "Percent difference\nin mortality rate") +
  coord_cartesian(ylim = c(-0.015, 0.06)) +
  facet_wrap(~model, nrow = 3)










##### 2. choice of supressed values


### impute to 0

allcause_fepois_model_imp1a <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 1) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                   data = df_allcause %>% mutate(all_deaths_allages = ifelse(all_deaths_allages < 10, 0, all_deaths_allages)),
                                   family = quasipoisson,
                                   warn = F,
                                   notes = F)
allcause_fepois_model_imp1b <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 6) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                    data = df_allcause %>% mutate(all_deaths_allages = ifelse(all_deaths_allages < 10, 0, all_deaths_allages)),
                                    family = quasipoisson,
                                    warn = F,
                                    notes = F)


### impute to 9

allcause_fepois_model_imp2a <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 1) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                     data = df_allcause %>% mutate(all_deaths_allages = ifelse(all_deaths_allages < 10, 9, all_deaths_allages)),
                                     family = quasipoisson,
                                     warn = F,
                                     notes = F)
allcause_fepois_model_imp2b <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 6) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                    data = df_allcause %>% mutate(all_deaths_allages = ifelse(all_deaths_allages < 10, 9, all_deaths_allages)),
                                    family = quasipoisson,
                                    warn = F,
                                    notes = F)

### impute to 4.5 - main model

allcause_fepois_model_imp3a <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 1) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                     data = df_allcause %>% mutate(all_deaths_allages = ifelse(all_deaths_allages < 10, 4.5, all_deaths_allages)),
                                     family = quasipoisson,
                                     warn = F,
                                     notes = F)
allcause_fepois_model_imp3b <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 6) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                     data = df_allcause %>% mutate(all_deaths_allages = ifelse(all_deaths_allages < 10, 4.5, all_deaths_allages)),
                                     family = quasipoisson,
                                     warn = F,
                                     notes = F)

### drop <10

allcause_fepois_model_dropa <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 1) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                    data = df_allcause %>% filter(all_deaths_allages >= 10),
                                    family = quasipoisson,
                                    warn = F,
                                    notes = F)
allcause_fepois_model_dropb <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 6) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                    data = df_allcause %>% filter(all_deaths_allages >= 10),
                                    family = quasipoisson,
                                    warn = F,
                                    notes = F)

### any county with missing

df_allcause %>% 
  mutate(dropper = ifelse(all_deaths_allages < 10, 1, 0)) %>%
  group_by(GEOID) %>%
  mutate(sum_dropper = sum(dropper, na.rm = T)) %>%
  ungroup() %>%
  filter(sum_dropper == 0) %>%
  count(GEOID)

allcause_fepois_model_dropalla <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 1) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                     data = df_allcause %>% 
                                       mutate(dropper = ifelse(all_deaths_allages < 10, 1, 0)) %>%
                                       group_by(GEOID) %>%
                                       mutate(sum_dropper = sum(dropper, na.rm = T)) %>%
                                       ungroup() %>%
                                       filter(sum_dropper == 0),
                                     family = quasipoisson,
                                     warn = F,
                                     notes = F)
allcause_fepois_model_dropallb <- feglm(all_deaths_allages ~ relevel(mean_rate_abas, 6) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population)) | state_year + GEOID,
                                     data = df_allcause %>% 
                                       mutate(dropper = ifelse(all_deaths_allages < 10, 1, 0)) %>%
                                       group_by(GEOID) %>%
                                       mutate(sum_dropper = sum(dropper, na.rm = T)) %>%
                                       ungroup() %>%
                                       filter(sum_dropper == 0),
                                     family = quasipoisson,
                                     warn = F,
                                     notes = F)


df_sens2 <- tibble(
  model = c(rep("imp0", 10),rep("imp9", 10),rep("imp4.5", 10),rep("drop", 10), rep("dropall", 10)),
  strata = rep(c(rep("no", 5), rep("any", 5)), 5),
  ambient = rep(c("(-Inf,0.1]", "(0.1,0.25]", "(0.25,0.5]", "(0.5,0.75]", "(0.75,1]"), 10),
  beta = c(0, allcause_fepois_model_imp1a$coeftable$Estimate[1:4],
           0, allcause_fepois_model_imp1b$coeftable$Estimate[6:9],
           0, allcause_fepois_model_imp2a$coeftable$Estimate[1:4],
           0, allcause_fepois_model_imp2b$coeftable$Estimate[6:9],
           0, allcause_fepois_model_imp3a$coeftable$Estimate[1:4],
           0, allcause_fepois_model_imp3b$coeftable$Estimate[6:9],
           0, allcause_fepois_model_dropa$coeftable$Estimate[1:4],
           0, allcause_fepois_model_dropb$coeftable$Estimate[6:9],
           0, allcause_fepois_model_dropalla$coeftable$Estimate[1:4],
           0, allcause_fepois_model_dropallb$coeftable$Estimate[6:9]),
  se = c(0, allcause_fepois_model_imp1a$coeftable$`Std. Error`[1:4],
           0, allcause_fepois_model_imp1b$coeftable$`Std. Error`[6:9],
           0, allcause_fepois_model_imp2a$coeftable$`Std. Error`[1:4],
           0, allcause_fepois_model_imp2b$coeftable$`Std. Error`[6:9],
           0, allcause_fepois_model_imp3a$coeftable$`Std. Error`[1:4],
           0, allcause_fepois_model_imp3b$coeftable$`Std. Error`[6:9],
           0, allcause_fepois_model_dropa$coeftable$`Std. Error`[1:4],
           0, allcause_fepois_model_dropb$coeftable$`Std. Error`[6:9],
         0, allcause_fepois_model_dropalla$coeftable$`Std. Error`[1:4],
         0, allcause_fepois_model_dropallb$coeftable$`Std. Error`[6:9])
) %>%
  mutate(rr = exp(beta)-1, ll = exp(beta-2.5*se)-1, ul = exp(beta+2.5*se)-1) %>%
  mutate(ambient = factor(ambient, 
                          levels = c("(-Inf,0.1]", "(0.1,0.25]", "(0.25,0.5]", "(0.5,0.75]", "(0.75,1]"),
                          labels = c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75+"))) %>%
  mutate(strata = factor(strata, levels = c("no", "any"), labels = c("No work exposure", "Work exposure"))) %>%
  mutate(model = factor(model,
                        levels = c("imp4.5", "imp0", "imp9", "drop", "dropall"),
                        labels = c("Main model", "Impute 0", "Impute 9", "Drop <10", "Drop county")))

df_sens2 %>%
  ggplot(aes(x = ambient, group = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_errorbar(aes(ymin = ll, ymax = ul), width = 0, color = "black") +
  geom_point(aes(y = rr,), color = "black", position=position_dodge(width=0.5), size = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = scales::percent) +
  theme_classic() +
  theme(panel.background = element_rect(color = "black", fill = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = NULL,
       y = "Percent difference\nin mortality rate") +
  coord_cartesian(ylim = c(-0.032, 0.032)) +
  facet_grid(model~strata)


df_sens2 %>%
  ggplot(aes(x = ambient, group = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_errorbar(aes(ymin = ll, ymax = ul), width = 0, color = "grey", position=position_dodge(width=0.8)) +
  geom_point(aes(y = rr, shape = model), color = "black", position=position_dodge(width=0.8), size = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = scales::percent) +
  theme_classic() +
  theme(panel.background = element_rect(color = "black", fill = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent"),
        axis.line = element_blank(),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  labs(x = NULL,
       y = "Percent difference\nin mortality rate") +
  coord_cartesian(ylim = c(-0.032, 0.032)) +
  facet_grid(.~strata)



##### 3. using working population instead of total population

allcause_fepois_model_workpop <- feglm(all_deaths_workpop ~ relevel(mean_bins_abas, 1) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population_workpop)) | state_year + GEOID,
                                        data = df_allcause,
                                        family = quasipoisson,
                                        warn = F,
                                        notes = F)

allcause_fepois_model_workpopa <- feglm(all_deaths_workpop ~ relevel(mean_rate_abas, 1) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population_workpop)) | state_year + GEOID,
                                     data = df_allcause,
                                     family = quasipoisson,
                                     warn = F,
                                     notes = F)

allcause_fepois_model_workpopb <- feglm(all_deaths_workpop ~ relevel(mean_rate_abas, 6) + splines::ns(mean_temp, df = 5) + splines::ns(precip_inches, df = 5) + offset(log(population_workpop)) | state_year + GEOID,
                                     data = df_allcause,
                                     family = quasipoisson,
                                     warn = F,
                                     notes = F)

df_sens_workpop_ambient <- tibble(
  ambient = rep(c("(-Inf,0.1]", "(0.1,0.25]", "(0.25,0.5]", "(0.5,0.75]", "(0.75,1]"), 1),
  beta = c(0, allcause_fepois_model_workpop$coeftable$Estimate[1:4]),
  se = c(0, allcause_fepois_model_workpop$coeftable$`Std. Error`[1:4])
) %>%
  mutate(rr = exp(beta)-1, ll = exp(beta-2.5*se)-1, ul = exp(beta+2.5*se)-1) %>%
  mutate(ambient = factor(ambient, 
                          levels = c("(-Inf,0.1]", "(0.1,0.25]", "(0.25,0.5]", "(0.5,0.75]", "(0.75,1]"),
                          labels = c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75+")))

df_sens_workpop_diff <- tibble(
  strata = c(rep("no", 5), rep("any", 5)),
  ambient = rep(c("(-Inf,0.1]", "(0.1,0.25]", "(0.25,0.5]", "(0.5,0.75]", "(0.75,1]"), 2),
  beta = c(0, allcause_fepois_model_workpopa$coeftable$Estimate[1:4],
           0, allcause_fepois_model_workpopb$coeftable$Estimate[6:9]),
  se = c(0, allcause_fepois_model_workpopa$coeftable$`Std. Error`[1:4],
         0, allcause_fepois_model_workpopb$coeftable$`Std. Error`[6:9])
) %>%
  mutate(rr = exp(beta)-1, ll = exp(beta-2.5*se)-1, ul = exp(beta+2.5*se)-1) %>%
  mutate(ambient = factor(ambient, 
                          levels = c("(-Inf,0.1]", "(0.1,0.25]", "(0.25,0.5]", "(0.5,0.75]", "(0.75,1]"),
                          labels = c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75+"))) %>%
  mutate(strata = factor(strata, levels = c("no", "any"), labels = c( "No work exposure", "Work exposure")))

sensfig3_1 <- df_sens_workpop_ambient %>%
  mutate(title = "Not stratified by work") %>%
  ggplot(aes(x = ambient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  #geom_errorbar(aes(ymin = ll, ymax = ul, group = title, color = ambient), width = 0) +
  geom_ribbon(aes(ymin = ll, ymax = ul, group = title), color = "grey90", alpha = 0.2)+
  geom_line(aes(y = rr, group = title), color = "black") +
  geom_point(aes(y = rr, group = title), color = "black") +
  #geom_point(aes(y = rr, group = title, color = ambient)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) +
  scale_color_manual(values = c("grey60", rev(met.brewer("Greek")))) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.1),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.minor.ticks.y.left = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.minor.ticks.length = unit(0.07, "cm"),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = NULL,
       y = "Percent difference in\nall-cause mortality rate") +
  coord_cartesian(ylim = c(-0.042, 0.042)) +
  facet_wrap(~title, nrow = 1)

sensfig3_23 <- df_sens_workpop_diff %>%
  ggplot(aes(x = ambient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  #geom_errorbar(aes(ymin = ll, ymax = ul, group = work, color = ambient), width = 0) +
  geom_ribbon(aes(ymin = ll, ymax = ul, group = strata, fill = strata), alpha = 0.2)+
  geom_line(aes(y = rr, group = strata, color = strata)) +
  geom_point(aes(y = rr, group = strata, color = strata)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) +
  scale_color_manual(values = c(met.brewer("Homer1")[c(4, 2)])) +
  scale_fill_manual(values = c(met.brewer("Homer1")[c(4, 2)])) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.1),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.ticks = element_line(color = "black"),
        axis.minor.ticks.y.left = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.minor.ticks.length = unit(0.07, "cm"),
        legend.position = "none") +
  labs(x = NULL,
       y = "Percent difference in\nall-cause mortality rate") +
  coord_cartesian(ylim = c(-0.042, 0.042)) +
  facet_wrap(~strata, nrow = 1)

ggarrange(
  sensfig3_1 + theme(plot.margin = unit(c(0, 5.5, 5.5, 5.5), 'points')), 
  sensfig3_23 + theme(plot.margin = unit(c(0, 5.5, 5.5, 5.5), 'points')),
  nrow = 1, widths = c(0.33, 0.6)
)












### supplemental figure of proportion of outdoor workers by county-year

sf_county_prop_outdoors <- us_counties %>% 
  st_transform("EPSG:3082") %>%
  full_join(county_outdoors_year, by = c("GEOID")) %>%
  mutate(perc_outdoors = ifelse(is.na(perc_outdoors), 0, perc_outdoors)) %>%
  filter(!(STATEFP %in% c("02", "15")))

county_outdoors_year$perc_outdoors %>% hist()

sf_county_prop_outdoors %>%
  #filter(year == 2006) %>%
  ggplot() +
  geom_sf(data = us_states, color = NA, fill = "black") +
  geom_sf(aes(fill = n_outdoors, color = n_outdoors), linewidth = 0.1) +
  geom_sf(data = us_states, color = "grey50", fill = NA) +
  geom_sf(data = us_states %>% filter(STUSPS %in% c("CA", "OR", "WA")), color = "white", 
          fill = NA, linewidth = 0.4) +
  facet_wrap(~year, nrow = 4) +
  scale_color_gradientn(colors=rev(c(met.brewer("Tam"), "#210e2e")),
                        na.value = "#210e2e",
                        labels = scales::unit_format(unit = "k", scale = 1e-3),
                        #breaks = c(5, 10, 15, 20, 25, 30, 35, 40),
                        name = "Likely number of\noutdoor workers",
                        values = scales::rescale(rgeoda::natural_breaks(k = 7, sf_county_prop_outdoors["n_outdoors"]))) +
  scale_fill_gradientn(colors=rev(c(met.brewer("Tam"), "#210e2e")),
                       na.value = "#210e2e",
                       labels = scales::unit_format(unit = "k", scale = 1e-3),
                       #breaks = c(5, 10, 15, 20, 25, 30, 35, 40),
                       name = "Likely number of\noutdoor workers",
                       values = scales::rescale(rgeoda::natural_breaks(k = 7, sf_county_prop_outdoors["n_outdoors"]))) +
  theme_void() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.72, 0.12),
        legend.direction = "horizontal",
        legend.key.width = unit(1.2, "cm"),
        legend.title = element_text(vjust = 1, hjust = 1))
