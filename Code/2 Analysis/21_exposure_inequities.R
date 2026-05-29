# Title: Wildfire smoke exposure inequities
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: May 5, 2026

# Description
# ----------- examine exposure inequities by race and ethnicity 
# ----------- for both workplace and ambient smoke exposure

setwd("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Exposure Lab (local)/Projects/PhD work/Analysis/2 Workplace Wildfire Smoke/")

library(tidyverse)
library(readxl)
library(lubridate)
library(MetBrewer)
library(ggpubr)
library(mgcv)

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

# read in workplace wildfire smoke exposure rates
# by county-year from 2006-2019
wildfire_exp_year <- readRDS("Data/3 Generated Data/County Exposure Estimates/wildfire_exp_county_year_2006_2019.rds")

wildfire_exp_year <- wildfire_work_year %>%
  mutate(year = year(Date)) %>%
  left_join(occ_county_year, by = c("GEOID", "year", "major_soc")) %>%
  mutate(exp_wildfire = estimate*exp_wildfire) %>%
  group_by(GEOID, year) %>%
  summarise(exp_wildfire = sum(exp_wildfire, na.rm = T)) %>%
  ungroup() %>%
  full_join(occ_county_year %>% group_by(GEOID, year) %>% summarise(workpop = sum(estimate, na.rm = T)) %>% ungroup(), by = c("GEOID", "year")) %>%
  mutate(rate_wildfire_per10000 = exp_wildfire/(workpop*250)*10000,
         rate_wildfire_per10000 = ifelse(is.na(rate_wildfire_per10000), 0, rate_wildfire_per10000))

# read in sociodemographic characteristics by county-year

### for 2006
sociodemo_county_year <- readRDS(paste0("Data/1 Input Data/ACS/acs_sociodemo_county_", 2007, ".rds")) %>%
  mutate(year = 2006) %>%
  mutate(allother_pct = 100-nhWhite_pct-nhBlack_pct-nhAsian_pct-hispanic_pct) %>%
  select(GEOID, totalpop, nhWhite_pct, nhBlack_pct, nhAsian_pct, hispanic_pct, allother_pct, minor_pct, unemployed_pct, less15pop_pct, greater64pop_pct, year)

### for 2007-2019
years_vector <- 2007:2019
#sociodemo_tract_year <- NULL
for(i in 1:length(years_vector)) {
  sociodemo_county_year <- rbind(sociodemo_county_year, 
                                readRDS(paste0("Data/1 Input Data/ACS/acs_sociodemo_county_", years_vector[i], ".rds")) %>%
                                  mutate(year = years_vector[i]) %>%
                                  mutate(allother_pct = 100-nhWhite_pct-nhBlack_pct-nhAsian_pct-hispanic_pct) %>%
                                  select(GEOID, totalpop, nhWhite_pct, nhBlack_pct, nhAsian_pct, hispanic_pct, allother_pct, minor_pct, unemployed_pct, less15pop_pct, greater64pop_pct, year))
}


# estimate rate of ambient wildfire smoke exposure by county-year
nonwork_wildfire_exp_year <- wildfire %>%
  mutate(year = year(Date)) %>%
  filter(year >= 2006 & year <= 2019) %>%
  left_join(sociodemo_county_year %>% select(GEOID, year, totalpop), by = c("GEOID", "year")) %>%
  group_by(GEOID, year) %>%
  summarise(nonwork_nexp = sum(totalpop)) %>%
  ungroup() %>%
  left_join(sociodemo_county_year %>% select(GEOID, year, totalpop), by = c("GEOID", "year")) %>%
  mutate(nonwork_rate = nonwork_nexp/(totalpop*365)*1000) %>%
  select(GEOID, year, nonwork_nexp, nonwork_rate, pop = totalpop)

# read in shapefile of census tracts to generate
# popultion-weighted county centroids by lat/lon
tracts_usa <- read_sf("1 Input Data/Tract Centroids/census_tract_centroid_2010.rds")

# a little processing on the shapefile
### assign tract centroids
county_centroids <- st_centroid(us_counties) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  as_tibble() %>%
  dplyr::select(GEOID, lon, lat)

# get county centroids
pwm_county_centroids <- sociodemo_tract_year %>%
  group_by(GEOID10) %>%
  summarise(mean_totalpop = mean(totalpop, na.rm = T)) %>%
  ungroup() %>%
  left_join(tract_centroids, by = c("GEOID10" = "GEOID")) %>%
  mutate(GEOID5 = str_extract(GEOID10, "^\\d{5}")) %>%
  mutate(w_lon = ifelse(is.na(mean_totalpop), lon, mean_totalpop*lon),
         w_lat = ifelse(is.na(mean_totalpop), lat, mean_totalpop*lat)) %>%
  group_by(GEOID5) %>%
  summarise(lon = sum(w_lon, na.rm = TRUE)/sum(mean_totalpop, na.rm = TRUE),
            lat = sum(w_lat, na.rm = TRUE)/sum(mean_totalpop, na.rm = TRUE))







# create dataset to analyze exposure inequities
example_wild_df <- expand_grid(
  GEOID = unique(sociodemo_county_year$GEOID), # unique county-years
  year = 2006:2019
) %>%
  left_join(
    sociodemo_county_year, by = c("GEOID", "year") # sociodemo characteristics
  ) %>%
  left_join(
    wildfire_exp_year, by = c("GEOID", "year") # workplace exposures
  ) %>%
  left_join(
    nonwork_wildfire_exp_year, by = c("GEOID", "year") # ambient exposures
  ) %>%
  left_join(county_centroids, by = c("GEOID")) %>% # county centroids
  mutate(exp_wildfire = ifelse(is.na(exp_wildfire), 0, exp_wildfire)) %>%
  mutate(exp_wildfire = round(exp_wildfire, 0)) %>%
  mutate(workpop = round(workpop, 0)) %>%
  mutate(nonwork_nexp = ifelse(is.na(nonwork_nexp), 0, nonwork_nexp)) %>%
  mutate(rate_wildfire_per10000 = ifelse(is.na(rate_wildfire_per10000), 0, rate_wildfire_per10000)) %>%
  mutate(nonwork_rate = ifelse(is.na(nonwork_rate), 0, nonwork_rate)) %>%
  mutate(state_fips = str_extract(GEOID, "^\\d\\d")) %>%
  filter(!(state_fips %in% c("02", "15"))) %>%
  filter(lon != 0) %>%
  filter(lat != 0) %>%
  na.omit() %>%
  filter(workpop > 0) # filter for counties with at least 1 worker




# run gam on exposure inequities by racial/ethnic minority status

### ambient
nonworkexp_gam_model <- example_wild_df %>%
  mgcv::gam(nonwork_nexp ~ s(minor_pct, fx=TRUE, k=3) + as.factor(year) + s(lon, lat, fx=TRUE, k=71),
            offset = log(pop),
            family = poisson(link = "log"),
            data = .)
summary(nonworkexp_gam_model)

### workplace
workexp_gam_model <- example_wild_df %>%
  mgcv::gam(exp_wildfire ~ s(minor_pct, fx=TRUE, k=3) + unemployed_pct + less15pop_pct + greater64pop_pct + as.factor(year) + s(lon, lat, fx=TRUE, k=71),
            offset = log(pop),
            family = poisson(link = "log"),
            data = .)
summary(workexp_gam_model)



pred_minor_wild_df <- tibble(minor_pct = seq(0, 
                                             quantile(example_wild_df$minor_pct, probs = 1), 
                                             by = 1),
                             less15pop_pct = 0, greater64pop_pct = 0, unemployed_pct = 0,
                             year = "2019", lat = median(example_wild_df$lat), lon = median(example_wild_df$lon)) %>%
  bind_cols(as_tibble(
    predict(nonworkexp_gam_model, newdata = ., se=TRUE)
  )) %>% 
  mutate(var = "minor") %>%
  mutate(outcome = "nonwork") %>%
  rename("percent" = minor_pct) %>%
  group_by(var) %>%
  mutate(fit = fit - fit[which.min(percent)]) %>% # adjust to the null
  ungroup() %>%
  
  rbind(
    tibble(minor_pct = seq(0, 
                           quantile(example_wild_df$minor_pct, probs = 1), 
                           by = 1),
           less15pop_pct = 0, greater64pop_pct = 0, unemployed_pct = 0,
           year = "2019", lat = median(example_wild_df$lat), lon = median(example_wild_df$lon)) %>%
      bind_cols(as_tibble(
        predict(workexp_gam_model, newdata = ., se=TRUE)
      )) %>% 
      mutate(var = "minor") %>%
      mutate(outcome = "work") %>%
      rename("percent" = minor_pct) %>%
      group_by(var) %>%
      mutate(fit = fit - fit[which.min(percent)]) %>% # adjust to the null
      ungroup()
  ) %>%
  
  select(var, outcome, percent, fit, se.fit) %>%
  mutate(fit = -1*as.numeric(fit),
         se.fit = as.numeric(se.fit)) %>%
  mutate(se_factor = 1.96) %>%
  mutate(ll = exp(fit - se_factor*se.fit)-1,
         ul = exp(fit + se_factor*se.fit)-1,
         irr = exp(fit)-1)


pred_minor_wild_df %>%
  ggplot(aes(x=percent/100, y=irr, group = outcome)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "lightgrey") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=.15) +
  geom_line(aes(linetype = outcome), linewidth = 0.7) +
  #geom_point(aes(shape = var), size = 1.5) +
  annotate("text", x = 0.8, y = -0.35, label = "Work", hjust = 1, size = 3) +
  annotate("text", x = 0.8, y = 0.2, label = "Ambient", hjust = 1, size = 3) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid = element_blank(), 
        legend.position = "none") +
  labs(x = "% White", y = "% increase in pop. exposed") +
  coord_cartesian(ylim = c(-0.4, 0.4), xlim = c(0, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent)



# run gam using specific race/ethnicity groups

### ambient
nonworkexp_gam_model_subset <- example_wild_df %>%
  mgcv::gam(nonwork_nexp ~ s(nhBlack_pct, fx=TRUE, k=3) + s(hispanic_pct, fx=TRUE, k=3) + s(nhAsian_pct, fx=TRUE, k=3) + s(allother_pct, fx=TRUE, k=3) + 
              as.factor(year) + s(lon, lat, fx=TRUE, k=71),
            offset = log(pop),
            family = poisson(link = "log"),
            data = .)
summary(nonworkexp_gam_model)

### workplace
workexp_gam_model_subset <- example_wild_df %>%
  mgcv::gam(exp_wildfire ~ s(nhBlack_pct, fx=TRUE, k=3) + s(hispanic_pct, fx=TRUE, k=3) + s(nhAsian_pct, fx=TRUE, k=3) + s(allother_pct, fx=TRUE, k=3) + 
              unemployed_pct + less15pop_pct + greater64pop_pct +
              as.factor(year) + s(lon, lat, fx=TRUE, k=71),
            offset = log(workpop),
            family = poisson(link = "log"),
            data = .)
summary(workexp_gam_model)



pred_nhBlack_wild_df <- tibble(nhBlack_pct = seq(0, quantile(example_wild_df$nhBlack_pct, probs = 1), by = 1),
                               hispanic_pct = 0, nhAsian_pct = 0, allother_pct = 0,
                               less15pop_pct = 0, greater64pop_pct = 0, unemployed_pct = 0,
                               year = "2019", lat = median(example_wild_df$lat), lon = median(example_wild_df$lon)) %>%
  bind_cols(as_tibble(
    predict(nonworkexp_gam_model_subset, newdata = ., se=TRUE)
  )) %>% 
  mutate(var = "minor") %>%
  mutate(outcome = "nonwork") %>%
  rename("percent" = nhBlack_pct) %>%
  group_by(var) %>%
  mutate(fit = fit - fit[which.min(percent)]) %>% # adjust to the null
  ungroup() %>%
  
  rbind(
    tibble(nhBlack_pct = seq(0, quantile(example_wild_df$nhBlack_pct, probs = 1), by = 1),
           hispanic_pct = 0, nhAsian_pct = 0, allother_pct = 0,
           less15pop_pct = 0, greater64pop_pct = 0, unemployed_pct = 0,
           year = "2019", lat = median(example_wild_df$lat), lon = median(example_wild_df$lon)) %>%
      bind_cols(as_tibble(
        predict(workexp_gam_model_subset, newdata = ., se=TRUE)
      )) %>% 
      mutate(var = "minor") %>%
      mutate(outcome = "work") %>%
      rename("percent" = nhBlack_pct) %>%
      group_by(var) %>%
      mutate(fit = fit - fit[which.min(percent)]) %>% # adjust to the null
      ungroup()
  ) %>%
  
  select(var, outcome, percent, fit, se.fit) %>%
  mutate(fit = as.numeric(fit),
         se.fit = as.numeric(se.fit)) %>%
  mutate(se_factor = ifelse(outcome == "nonwork", 1000, 2)) %>%
  mutate(ll = exp(fit - se_factor*se.fit)-1,
         ul = exp(fit + se_factor*se.fit)-1,
         irr = exp(fit)-1)


pred_nhBlack_wild_df %>%
  ggplot(aes(x=percent/100, y=irr, group = outcome)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "lightgrey") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=.15) +
  geom_line(aes(linetype = outcome), linewidth = 0.7) +
  #geom_point(aes(shape = var), size = 1.5) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid = element_blank(), 
        legend.position = "none") +
  labs(x = "% non-Hispanic Black", y = "% increase in pop. exposed") +
  coord_cartesian(ylim = c(-0.4, 0.6), xlim = c(0, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent)


example_wild_df$hispanic_pct %>% median

pred_hispanic_wild_df <- tibble(nhBlack_pct = 0,
                                hispanic_pct = seq(0, quantile(example_wild_df$hispanic_pct, probs = 1), by = 1), 
                                nhAsian_pct = 0, allother_pct = 0,
                                less15pop_pct = 0, greater64pop_pct = 0, unemployed_pct = 0,
                                year = "2019", lat = median(example_wild_df$lat), lon = median(example_wild_df$lon)) %>%
  bind_cols(as_tibble(
    predict(nonworkexp_gam_model_subset, newdata = ., se=TRUE)
  )) %>% 
  mutate(var = "minor") %>%
  mutate(outcome = "nonwork") %>%
  rename("percent" = hispanic_pct) %>%
  group_by(var) %>%
  mutate(fit = fit - fit[which.min(percent)]) %>% # adjust to the null
  ungroup() %>%
  
  rbind(
    tibble(nhBlack_pct = 0,
           hispanic_pct = seq(0, quantile(example_wild_df$hispanic_pct, probs = 1), by = 1), 
           nhAsian_pct = 0, allother_pct = 0,
           less15pop_pct = 0, greater64pop_pct = 0, unemployed_pct = 0,
           year = "2019", lat = median(example_wild_df$lat), lon = median(example_wild_df$lon)) %>%
      bind_cols(as_tibble(
        predict(workexp_gam_model_subset, newdata = ., se=TRUE)
      )) %>% 
      mutate(var = "minor") %>%
      mutate(outcome = "work") %>%
      rename("percent" = hispanic_pct) %>%
      group_by(var) %>%
      mutate(fit = fit - fit[which.min(percent)]) %>% # adjust to the null
      ungroup()
  ) %>%
  
  select(var, outcome, percent, fit, se.fit) %>%
  mutate(fit = as.numeric(fit),
         se.fit = as.numeric(se.fit)) %>%
  mutate(se_factor = ifelse(outcome == "nonwork", 1000, 20)) %>%
  mutate(ll = exp(fit - se_factor*se.fit)-1,
         ul = exp(fit + se_factor*se.fit)-1,
         irr = exp(fit)-1)


pred_hispanic_wild_df %>%
  ggplot(aes(x=percent/100, y=irr, group = outcome)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "lightgrey") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=.15) +
  geom_line(aes(linetype = outcome), linewidth = 0.7) +
  #geom_point(aes(shape = var), size = 1.5) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid = element_blank(), 
        legend.position = "none") +
  labs(x = "% Hispanic", y = "% increase in pop. exposed") +
  #coord_cartesian(ylim = c(-0.4, 0.6), xlim = c(0, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent)







## final figures for 2a



fig1_race_a <- pred_minor_wild_df %>%
  ggplot(aes(x=percent/100, y=irr, group = outcome)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "lightgrey") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=.15) +
  geom_line(aes(linetype = outcome), linewidth = 0.7) +
  #geom_point(aes(shape = var), size = 1.5) +
  annotate("text", x = 0.8, y = -0.28, label = "Work", hjust = 1, size = 3) +
  annotate("text", x = 0.8, y = 0.2, label = "Ambient", hjust = 1, size = 3) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid = element_blank(), 
        legend.position = "none") +
  labs(x = "% non-Hispanic White", y = "% Δ in pop. exposed") +
  coord_cartesian(ylim = c(-0.4, 0.4), xlim = c(0, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.1),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.minor.ticks.y.left = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.minor.ticks.length = unit(0.07, "cm"),
        legend.position = "none")


fig1_race_a

fig1_race_b <- pred_nhBlack_wild_df %>%
  ggplot(aes(x=percent/100, y=irr, group = outcome)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "lightgrey") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=.15) +
  geom_line(aes(linetype = outcome), linewidth = 0.7) +
  #geom_point(aes(shape = var), size = 1.5) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid = element_blank(), 
        legend.position = "none") +
  labs(x = "% non-Hispanic Black", y = "% Δ in pop. exposed") +
  coord_cartesian(ylim = c(-0.4, 0.4), xlim = c(0, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) + 
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.1),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.minor.ticks.y.left = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.minor.ticks.length = unit(0.07, "cm"),
        legend.position = "none")


fig1_race_b


fig1_race_c <- pred_hispanic_wild_df %>%
  ggplot(aes(x=percent/100, y=irr, group = outcome)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "lightgrey") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=.15) +
  geom_line(aes(linetype = outcome), linewidth = 0.7) +
  annotate("text", x = 0, y = 5, label = "*Note scale change", size = 3, hjust = 0) +
  #geom_point(aes(shape = var), size = 1.5) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid = element_blank(), 
        legend.position = "none") +
  labs(x = "% Hispanic", y = "% Δ in pop. exposed") +
  coord_cartesian(ylim = c(-0.4, 5), xlim = c(0, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::percent) + 
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.1),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.minor.ticks.y.left = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.minor.ticks.length = unit(0.07, "cm"),
        legend.position = "none")


fig1_race_c

fig_2a <- ggarrange(
  fig1_race_a + labs(y = "Percent difference in\npopulation exposed"), 
  fig1_race_b + labs(y = NULL), 
  fig1_race_c + labs(y = NULL), 
  nrow = 1
)

fig_2a




fig_2a <- pred_minor_wild_df %>% mutate(race = "Non-Hispanic White") %>%
  rbind(pred_nhBlack_wild_df %>% mutate(race = "Non-Hispanic Black")) %>%
  rbind(pred_hispanic_wild_df %>% mutate(race = "Hispanic")) %>%
  mutate(race = factor(race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic"))) %>%
  ggplot(aes(x=percent/100, y=irr, group = outcome)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "lightgrey") +
  geom_ribbon(aes(ymin=ll, ymax=ul), alpha=.15) +
  geom_line(aes(linetype = outcome), linewidth = 0.7) +
  geom_text(data = tibble(percent = 0, irr = 0.4, outcome = "nonwork", race = "Non-Hispanic White") %>%
              mutate(race = factor(race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic"))), 
            aes(label = race), color = NA) +
  geom_text(data = tibble(percent = 0, irr = -0.4, outcome = "nonwork", race = "Non-Hispanic White") %>%
              mutate(race = factor(race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic"))), 
            aes(label = race), color = NA) +
  geom_text(data = tibble(percent = 90, irr = 0.265, outcome = "nonwork", race = "Non-Hispanic White", label = "Ambient") %>%
              mutate(race = factor(race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic"))), 
            aes(label = label), size = 3, hjust = 1) +
  geom_text(data = tibble(percent = 90, irr = -0.325, outcome = "nonwork", race = "Non-Hispanic White", label = "Work") %>%
              mutate(race = factor(race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic"))), 
            aes(label = label), size = 3, hjust = 1) +
  geom_text(data = tibble(percent = 0, irr = 0.4, outcome = "nonwork", race = "Non-Hispanic Black") %>%
              mutate(race = factor(race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic"))), 
            aes(label = race), color = NA) +
  geom_text(data = tibble(percent = 0, irr = -0.4, outcome = "nonwork", race = "Non-Hispanic Black") %>%
              mutate(race = factor(race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic"))), 
            aes(label = race), color = NA) +
  geom_text(data = tibble(percent = 0, irr = 3.8, outcome = "nonwork", race = "Hispanic", label = "*Note scale change") %>%
              mutate(race = factor(race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic"))), 
            aes(label = label), size = 3, hjust = 0) +
  #annotate("text", x = 0, y = 5, label = "*Note scale change", size = 3, hjust = 0) +
  #geom_point(aes(shape = var), size = 1.5) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.grid = element_blank(), 
        legend.position = "none") +
  labs(x = "Percent of racial and ethnic group in county", y = "Percent difference in\npopulation exposed",
       title = "Racial and ethnic exposure inequities to wildfire smoke") +
  #coord_cartesian(ylim = c(-0.4, 0.4), xlim = c(0, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = scales::percent) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3), labels = scales::percent, guide = guide_axis(minor.ticks = T)) + 
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", linewidth = 1.1),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.minor.ticks.y.left = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.minor.ticks.length = unit(0.07, "cm"),
        legend.position = "none") +
  facet_wrap(~race, scales = "free_y")

fig_2a



