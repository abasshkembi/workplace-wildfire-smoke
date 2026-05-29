# Title: Workplace wildfire smoke descriptives
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: May 5, 2026

# Description
# ----------- calculate basic statistics on the number of workers exposed
# ----------- and visualize this in Figure 1

setwd("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Exposure Lab (local)/Projects/PhD work/Analysis/2 Workplace Wildfire Smoke/")


library(tidyverse)
library(readxl)
library(lubridate)
library(MetBrewer)
library(ggpubr)

# read in daily, county exposure proportions
wildfire_work_year <- NULL
for(i in 2006:2019) {
  temp_read <- readRDS(paste0("Data/3 Generated Data/County Proportions/daily_wildfire_prop_occ_county_", i, ".rds"))
  wildfire_work_year <- rbind(wildfire_work_year, temp_read)
}

# read in workplace wildfire smoke exposure rates
# by county-year from 2006-2019
wildfire_county_rates <- readRDS("Data/3 Generated Data/County Exposure Estimates/wildfire_exp_county_year_2006_2019.rds")


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





# total number of exposures
sum(wildfire_county_rates$exp_wildfire, na.rm = T)

# how many exposures are >35.5 ug/m3?

wildfire_work_year %>%
  mutate(year = year(Date)) %>%
  left_join(occ_county_year, by = c("GEOID", "year", "major_soc")) %>%
  group_by(ecc) %>%
  summarise(exp_wildfire = sum(exp_wildfire*estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(perc = exp_wildfire/sum(exp_wildfire))

836422148 - 797235557






## figure 1 - daily number of people exposed

daily_wildfire_df <- wildfire_work_year %>%
  mutate(year = year(Date)) %>%
  left_join(occ_county_year, by = c("GEOID", "year", "major_soc")) %>%
  group_by(Date) %>%
  summarise(exp_wildfire = sum(exp_wildfire*estimate, na.rm = TRUE)) %>%
  ungroup()

daily_wildfire_df %>% filter(Date >= "2010-01-01") %>% .$exp_wildfire %>% sum

daily_wildfire_df %>%
  filter(exp_wildfire >= 1e6)

daily_wildfire_df %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(sum = sum(exp_wildfire)) %>%
  ungroup() %>%
  mutate(perc = sum/sum(sum))


### create panel of daily number of workers exposed
fig1_a <- tibble(
  Date = seq(as.Date("2006-01-01"), as.Date("2019-12-31"), by="days")
) %>%
  left_join(daily_wildfire_df , by = "Date") %>%
  mutate(n_exp = ifelse(is.na(exp_wildfire), 0, exp_wildfire)) %>%
  mutate(cum_exp = cumsum(n_exp)) %>%
  ggplot(aes(x = Date)) +
  geom_area(aes(y = cum_exp*0.012), fill = "grey90") +
  geom_area(aes(y = n_exp), fill = "black") +
  geom_line(aes(y = n_exp), linewidth = 0.25) +
  annotate("text", x = as.Date("2019-12-01"), y = 8.25e6, label = "Cumulative number of\nperson-days (shaded)", color = "grey30", size = 3, hjust = 1) +
  theme_classic() +
  scale_x_date(date_labels = "%Y", name = "Date", date_breaks = "2 years") +
  scale_y_continuous(name = "Daily number of people\n(black line)", labels = scales::unit_format(unit = "M", scale = 1e-6),
                     breaks = c(0, 2e6, 4e6, 6e6, 8e6, 10e6),
                     sec.axis=sec_axis(~./0.012,
                                       #name="Cumulative number of person-days\n(shaded grey)", 
                                       labels = scales::unit_format(unit = "M", scale = 1e-6))
  ) + 
  labs(title = "Nationwide exposure to wildfire smoke at work, daily") +
  coord_cartesian(xlim = as.Date(c('2006-08-16','2019-05-16')),
                  ylim = c(4.2e5, 10e6))

fig1_a


### overall rate of wildfire smoke exposure
wildfire_county_rates$exp_wildfire %>% sum(na.rm = T)
occ_county_year$estimate %>% sum(na.rm = T)
836422148/(2058885715*250)*10000

### overall rate by major occupational groups
wildfire_work_year %>%
  mutate(year = year(Date)) %>%
  left_join(occ_county_year, by = c("GEOID", "year", "major_soc")) %>%
  mutate(exp_wildfire = estimate*exp_wildfire) %>%
  group_by(major_soc) %>%
  summarise(n_exp = sum(exp_wildfire, na.rm = T)) %>%
  ungroup() %>%
  left_join(occ_county_year %>% 
              group_by(major_soc) %>% 
              summarise(workpop = sum(estimate, na.rm = T)), by = c("major_soc")) %>%
  mutate(rate = round(n_exp/(workpop*250)*10000, 1)) %>%
  arrange(-rate) %>%
  print(n = 22)

### prep rate by major occupational groups for another figure panel
soc_df_fig1 <- wildfire_work_year %>%
  mutate(year = year(Date)) %>%
  left_join(occ_county_year, by = c("GEOID", "year", "major_soc")) %>%
  mutate(exp_wildfire = estimate*exp_wildfire) %>%
  mutate(major_soc = ifelse(!(major_soc %in% paste0(c("47", "53", "49", "37", "33", "51", "11", "41", "45", "43", "39", "25", "17", "27", "19"), "-0000")), "Others", major_soc)) %>%
  group_by(major_soc) %>%
  summarise(n_exp = sum(exp_wildfire, na.rm = T)) %>%
  ungroup() %>%
  left_join(occ_county_year %>% 
              mutate(major_soc = ifelse(!(major_soc %in% paste0(c("47", "53", "49", "37", "33", "51", "11", "41", "45", "43", "39", "25", "17", "27", "19"), "-0000")), "Others", major_soc)) %>%
              group_by(major_soc) %>% 
              summarise(workpop = sum(estimate, na.rm = T)), by = c("major_soc")) %>%
  mutate(rate = round(n_exp/(workpop*250)*10000, 1)) %>%
  mutate(major_soc = str_remove(major_soc, "-0000")) %>%
  mutate(n_exp = scales::label_number(scale = 1/1e6)(ceiling(n_exp))) %>%
  mutate(n_exp = ifelse(major_soc == "51", paste0(round(as.numeric(n_exp), 0), " M person-days"), paste0(round(as.numeric(n_exp), 0), " M"))) 

soc_df_fig1

### generate figure panel of major occupational exposure rate
fig1_b <- soc_df_fig1 %>%
  ggplot(aes(y = rate, x = fct_reorder(major_soc, rate))) +
  geom_col(fill = "#f5c34d") +
  geom_hline(yintercept = 16.25, linetype = "dashed", color = "grey30") +
  annotate("text", label = "Nationwide rate (16.3 per 10,000)", x = 1, y = 18, size = 3, hjust = 0, vjust = 1, color = "grey30") +
  geom_label(aes(label = n_exp), label.size = NA, data = . %>% filter(!(major_soc %in% c("47", "51", "53"))), nudge_y = 6.5, size = 3, fill = NA) +
  geom_label(aes(label = n_exp), label.size = NA, data = . %>% filter(major_soc %in% c("47", "53")), nudge_y = 7.5, size = 3, fill = NA) +
  geom_label(aes(label = n_exp), label.size = NA, data = . %>% filter(major_soc == "51"), nudge_y = 6, size = 3, hjust = 0.15, fill = NA) +
  scale_y_continuous(name = "Workplace exposure rate\n(per 10,000 workers)", 
                     breaks = scales::pretty_breaks(n = 4)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Major Occupational Groups", title = "Nationwide, by occupation") +
  coord_flip(ylim = c(4.1, 87))

fig1_b







# compare ambient vs workplace exposures at a daily level

daily_wildfire_GEOID <- wildfire_work_year %>%
  mutate(year = year(Date)) %>%
  left_join(occ_county_year, by = c("GEOID", "year", "major_soc")) %>%
  group_by(GEOID, Date) %>%
  summarise(n_exp = sum(exp_wildfire*estimate, na.rm = TRUE)) %>%
  ungroup()

fig1_c <- wildfire %>%
  mutate(year = year(Date)) %>%
  filter(year >= 2006 & year <= 2019) %>%
  left_join(
    county_workpop, by = c("GEOID", "year")
  ) %>%
  left_join(
    daily_wildfire_GEOID, by = c("GEOID", "Date")
  ) %>%
  filter(smokePM_pred >= 9) %>%
  mutate(rate = n_exp/workpop*100) %>%
  # mutate(rate_bins = cut(rate, breaks = seq(0, 400, by = 10))) %>%
  # mutate(smoke_bins = cut(smokePM_pred, breaks = seq(0, 300, by = 10))) %>%
  filter(GEOID != "46102") %>%
  mutate(rate = ifelse(is.na(n_exp), 0, rate)) %>%
  ggplot(aes(x = smokePM_pred, y = rate)) +
  stat_bin_2d(aes(fill = after_stat(count)), binwidth = c(log10(1.075),1.5)) +
  #geom_point(alpha = 0.1, shape = 1, height = 15) +
  stat_smooth(se = F, linetype = "solid", color = MetBrewer::met.brewer("Greek")[3], linewidth = 1, span = 0.05) +
  scale_x_continuous(breaks = c(9, 20, 50, 100, 200), trans = "log10",
                     guide = guide_axis(minor.ticks = T), minor_breaks = c(35, 75, 150, 250, 300, 350, 400, 450, 500)) +
  #scale_x_log10() +
  #scale_fill_grey() +
  scale_fill_gradient(low = "gray92", high = "gray10", trans = "sqrt") +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(10.62, 300), ylim = c(2.3, 50)) +
  labs(x = expression(paste("Ambient smoke ", PM[2.5], " ", ("µg" / m^3))), y = "Workplace exposure rate\n(per 100 workers)",
       title = "Ambient vs work, daily")

fig1_c

# compare ambient vs workplace exposures at a yearly level

yearly_ambient_vs_work <- wildfire %>%
  filter(Date >= "2006-01-01") %>% filter(Date <= "2019-12-31") %>%
  mutate(year = year(Date)) %>%
  group_by(GEOID, year) %>%
  summarise(mean_smokepm2.5 = sum(smokePM_pred, na.rm = T)/(365)) %>%
  ungroup() %>%
  full_join(
    wildfire_county_rates, by = c("GEOID", "year")
  ) %>%
  mutate(rate_wildfire_per10000 = ifelse(is.na(rate_wildfire_per10000), 0, rate_wildfire_per10000),
         mean_smokepm2.5 = ifelse(is.na(mean_smokepm2.5), 0, mean_smokepm2.5))

yearly_ambient_vs_work %>% 
  filter(rate_wildfire_per10000 == 0) %>%
  mutate(mean_smokepm2.5 = cut(mean_smokepm2.5, breaks = c(-Inf, 0.1, 0.25, 0.5, 0.75, 1, Inf))) %>%
  count(mean_smokepm2.5)

fig1_d <- yearly_ambient_vs_work %>%
  ggplot(aes(x = mean_smokepm2.5, y = rate_wildfire_per10000)) +
  stat_bin_2d(aes(fill = after_stat(count)), binwidth = c(0.2,20)) +
  #geom_point(alpha = 0.1, shape = 1, height = 15) +
  stat_smooth(se = F, linetype = "solid", color = MetBrewer::met.brewer("Greek")[3], linewidth = 1, span = 0.05) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  # scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10), trans = "log10",
  #                    guide = guide_axis(minor.ticks = T), minor_breaks = c(1, 3, 5, 6, 9)) +
  #scale_fill_grey() +
  scale_fill_gradient(low = "gray92", high = "gray10", trans = "sqrt") +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0.45, 10), ylim = c(28, 600)) +
  #coord_cartesian(xlim = c(0, 2), ylim = c(0, 200)) +
  labs(x = expression(paste("Ambient smoke ", PM[2.5], " ", ("µg" / m^3))), y = "Workplace exposure rate\n(per 10,000 workers)",
       title = "Ambient vs work, yearly")

fig1_d +
  coord_cartesian(xlim = c(0, 2))


### combine the panels into a single figure
ggarrange(
  ggarrange(
    fig1_a,
    ggarrange(
      fig1_c, fig1_d, nrow = 1
    ),
    nrow = 2
  ),
  fig1_b,
  nrow = 1, widths = c(0.7, 0.3)
)










# create map of ambient vs workplace exposures
# averaged across 2006-2019

### read in shapefile of US counties
unique_state_fips <- state_laea$GEOID
us_counties <- NULL
for(i in 1:length(unique_state_fips)) {
  state_tracts <- counties(state = unique_state_fips[i], cb = T, year = 2017)
  
  us_counties <- rbind(us_counties, state_tracts)
}

### prep the shapefile
us_states <- states(cb = T, year = 2017) %>%
  shift_geometry(geoid_column = "GEOID") %>%
  st_transform("EPSG:3082") %>%
  filter(STATEFP %in% unique_state_fips) %>%
  filter(!(STATEFP %in% c("02", "15")))


### daily, county, wildfire data - 2006-2020
wildfire <- readRDS("Data/1 Input Data/Wildfire/smokePM2pt5_predictions_daily_county_20060101-20201231.Rds") %>%
  rename(Date = date)
### average wildfire PM2.5 levels from 2006-2019
wildfire_2006_2019 <- wildfire %>%
  filter(Date >= "2006-01-01") %>% filter(Date <= "2019-12-31") %>%
  group_by(GEOID) %>%
  summarise(mean_smokepm2.5 = sum(smokePM_pred)/(365*14)) %>%
  ungroup()


### create dataset of workplace exposure rates by county for 2006-2019
sf_counties_work <- us_counties %>%
  full_join(wildfire_county_rates %>%
              group_by(GEOID) %>%
              summarise(exp_wildfire = sum(exp_wildfire, na.rm = T),
                        workpop = sum(workpop, na.rm = T)) %>%
              mutate(rate_wildfire_per10000 = exp_wildfire/(workpop*250)*10000) %>%
              select(GEOID, rate_wildfire_per10000), by = "GEOID") %>%
  full_join(wildfire_2006_2019, by = "GEOID") %>%
  mutate(rate_wildfire_per10000 = ifelse(is.na(rate_wildfire_per10000), 0, rate_wildfire_per10000),
         mean_smokepm2.5 = ifelse(is.na(mean_smokepm2.5), 0, mean_smokepm2.5)) %>%
  st_transform("EPSG:3082") %>%
  filter(!(STATEFP %in% c("02", "15")))





sf_counties_work$rate_wildfire_per10000 %>% quantile(probs = c(0, 0.3333, 0.66666, 1))
sf_counties_work$mean_smokepm2.5 %>% quantile(probs = c(0, 0.3333, 0.66666, 1), na.rm = T)

### prepare to make bivariate choropleth map

library(biscale)
library(cowplot)

data <- bi_class(sf_counties_work, x = mean_smokepm2.5, y = rate_wildfire_per10000, style = "quantile", dim = 3)
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class, color = bi_class), linewidth = 0.1, show.legend = F) +
  geom_sf(data = us_states, color = "grey50", fill = NA) +
  #geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_scale_color(pal = "GrPink", dim = 3) +
  bi_theme()

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Ambient ",
                    ylab = "Work ",
                    size = 8)

legend

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.1, 0.2, 0.2)


map

### figure 1e - bivariate choropleth map of workplace vs ambient 
### exposures averaged across 2006-2019

finalPlot

















#### supplemental figures


fig1_rate <- sf_counties_work %>%
  ggplot() +
  geom_sf(aes(fill = rate_wildfire_per10000, color = rate_wildfire_per10000), linewidth = 0.1) +
  geom_sf(data = us_states, color = "grey50", fill = NA) +
  geom_sf(data = us_states %>% filter(STUSPS %in% c("CA", "OR", "WA")), color = "white", 
          fill = NA, linewidth = 0.4) +
  scale_color_gradientn(colors=rev(c(met.brewer("Tam"), "#210e2e")),
                        na.value = "#210e2e",
                        breaks = c(10, 50, 100, 150),
                        name = "Workplace exposure rate\n(per 10,000 workers)",
                        values = scales::rescale(rgeoda::quantile_breaks(k = 5, sf_counties_work["rate_wildfire_per10000"]))) +
  scale_fill_gradientn(colors=rev(c(met.brewer("Tam"), "#210e2e")),
                       na.value = "#210e2e",
                       breaks = c(10, 50, 100, 150),
                       name = "Workplace exposure rate\n(per 10,000 workers)",
                       values = scales::rescale(rgeoda::quantile_breaks(k = 5, sf_counties_work["rate_wildfire_per10000"]))) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.title = element_text(vjust = 1, hjust = 1, size = 10),
        legend.text = element_text(size = 8, vjust = 1.5))

fig1_rate


fig1_ambient <- sf_counties_work %>%
  ggplot() +
  geom_sf(aes(fill = mean_smokepm2.5, color = mean_smokepm2.5), linewidth = 0.1) +
  geom_sf(data = us_states, color = "grey50", fill = NA) +
  geom_sf(data = us_states %>% filter(STUSPS %in% c("CA", "OR", "WA")), color = "white", 
          fill = NA, linewidth = 0.4) +
  scale_color_gradientn(colors=rev(c(met.brewer("Tam"), "#210e2e")),
                        na.value = "#210e2e",
                        #breaks = c(10, 50, 100, 150),
                        name = expression(paste("Ambient smoke ", PM[2.5], " ", ("µg" / m^3))),
                        values = scales::rescale(rgeoda::natural_breaks(k = 5, wildfire_2006_2019["mean_smokepm2.5"]))) +
  scale_fill_gradientn(colors=rev(c(met.brewer("Tam"), "#210e2e")),
                       na.value = "#210e2e",
                       #breaks = c(10, 50, 100, 150),
                       name = expression(paste("Ambient smoke ", PM[2.5], " ", ("µg" / m^3))),
                       values = scales::rescale(rgeoda::natural_breaks(k = 5, wildfire_2006_2019["mean_smokepm2.5"]))) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.title = element_text(vjust = 1, hjust = 1, size = 10),
        legend.text = element_text(size = 8, vjust = 1.5))

fig1_ambient









sf_county_workrate_year <- us_counties %>% 
  st_transform("EPSG:3082") %>%
  full_join(wildfire_county_rates, by = c("GEOID")) %>%
  mutate(rate_wildfire_per10000 = ifelse(is.na(rate_wildfire_per10000), 0, rate_wildfire_per10000)) %>%
  filter(!(STATEFP %in% c("02", "15")))

sf_county_workrate_year %>%
  as_tibble() %>%
  count(GEOID) %>% filter(n < 14)

sf_county_workrate_year %>%
  ggplot() +
  geom_sf(data = us_states, color = NA, fill = "black") +
  geom_sf(aes(fill = rate_wildfire_per10000, color = rate_wildfire_per10000), linewidth = 0.1) +
  geom_sf(data = us_states, color = "grey50", fill = NA) +
  geom_sf(data = us_states %>% filter(STUSPS %in% c("CA", "OR", "WA")), color = "white", 
          fill = NA, linewidth = 0.4) +
  facet_wrap(~year, nrow = 4) +
  scale_color_gradientn(colors=rev(c(met.brewer("Tam"), "#210e2e")),
                        na.value = "#210e2e",
                        breaks = c(10, 100, 200, 300, 400, 500, 600),
                        name = "Wildfire exposure rate\n(per 10,000 workers)",
                        values = scales::rescale(rgeoda::natural_breaks(k = 7, sf_county_workrate_year["rate_wildfire_per10000"]))) +
  scale_fill_gradientn(colors=rev(c(met.brewer("Tam"), "#210e2e")),
                       na.value = "#210e2e",
                       breaks = c(10, 100, 200, 300, 400, 500, 600),
                       name = "Wildfire exposure rate\n(per 10,000 workers)",
                       values = scales::rescale(rgeoda::natural_breaks(k = 7, sf_county_workrate_year["rate_wildfire_per10000"]))) +
  theme_void() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.72, 0.12),
        legend.direction = "horizontal",
        legend.key.width = unit(1.2, "cm"),
        legend.title = element_text(vjust = 1, hjust = 1))

sf_county_workrate_year %>%
  #filter(year == 2006) %>%
  mutate(work_bin = ifelse(rate_wildfire_per10000 > 0, "Yes", "No")) %>%
  ggplot() +
  geom_sf(data = us_states, color = NA, fill = met.brewer("Demuth")[c(8)]) +
  geom_sf(aes(fill = work_bin, color = work_bin), linewidth = 0.1) +
  geom_sf(data = us_states, color = "grey50", fill = NA) +
  geom_sf(data = us_states %>% filter(STUSPS %in% c("CA", "OR", "WA")), color = "white", 
          fill = NA, linewidth = 0.4) +
  facet_wrap(~year, nrow = 4) +
  scale_color_manual(values=met.brewer("Demuth")[c(8, 3)],
                     na.value = "grey",
                     name = "Workplace exposure") +
  scale_fill_manual(values=met.brewer("Demuth")[c(8, 3)],
                    na.value = "grey",
                    name = "Workplace exposure") +
  theme_void() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.72, 0.12),
        legend.direction = "horizontal",
        legend.key.width = unit(1.2, "cm"),
        legend.title = element_text(vjust = 0.5, hjust = 1))


sf_county_workrate_year %>%
  #filter(year == 2006) %>%
  filter(rate_wildfire_per10000 > 0) %>%
  left_join(wildfire_year_2006_2019, by = c("GEOID", "year")) %>%
  mutate(mean_smokepm2.5 = cut(mean_smokepm2.5, breaks = c(-Inf, 0.1, 0.25, 0.5, 0.75, Inf))) %>%
  mutate(mean_smokepm2.5 = factor(mean_smokepm2.5, labels = c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75+"))) %>%
  ggplot() +
  geom_sf(data = us_states, color = NA, fill = "grey90") +
  geom_sf(aes(fill = mean_smokepm2.5, color = mean_smokepm2.5), linewidth = 0.1) +
  geom_sf(data = us_states, color = "grey50", fill = NA) +
  geom_sf(data = us_states %>% filter(STUSPS %in% c("CA", "OR", "WA")), color = "white", 
          fill = NA, linewidth = 0.4) +
  facet_wrap(~year, nrow = 4) +
  scale_color_manual(values=met.brewer("Tam", n = 5),
                     na.value = "grey",
                     name = expression(paste("Ambient smoke ", PM[2.5], " ", ("µg" / m^3)))) +
  scale_fill_manual(values=met.brewer("Tam", n = 5),
                    na.value = "grey",
                    name = expression(paste("Ambient smoke ", PM[2.5], " ", ("µg" / m^3)))) +
  theme_void() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.65, 0.12),
        legend.direction = "vertical",
        legend.key.width = unit(0.8, "cm"),
        legend.title = element_text( hjust = 0.5))


sf_county_workrate_year %>%
  #filter(year == 2006) %>%
  filter(rate_wildfire_per10000 == 0) %>%
  left_join(wildfire_year_2006_2019, by = c("GEOID", "year")) %>%
  mutate(mean_smokepm2.5 = cut(mean_smokepm2.5, breaks = c(-Inf, 0.1, 0.25, 0.5, 0.75, Inf))) %>%
  mutate(mean_smokepm2.5 = factor(mean_smokepm2.5, labels = c("<0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75+"))) %>%
  as_tibble() %>%
  filter(is.na(mean_smokepm2.5)) %>%
  ggplot() +
  geom_sf(data = us_states, color = NA, fill = "grey90") +
  geom_sf(aes(fill = mean_smokepm2.5, color = mean_smokepm2.5), linewidth = 0.1) +
  geom_sf(data = us_states, color = "grey50", fill = NA) +
  geom_sf(data = us_states %>% filter(STUSPS %in% c("CA", "OR", "WA")), color = "white", 
          fill = NA, linewidth = 0.4) +
  facet_wrap(~year, nrow = 4) +
  scale_color_manual(values=met.brewer("Tam", n = 5),
                     na.value = "grey",
                     name = expression(paste("Ambient smoke ", PM[2.5], " ", ("µg" / m^3)))) +
  scale_fill_manual(values=met.brewer("Tam", n = 5),
                    na.value = "grey",
                    name = expression(paste("Ambient smoke ", PM[2.5], " ", ("µg" / m^3)))) +
  theme_void() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.65, 0.12),
        legend.direction = "vertical",
        legend.key.width = unit(0.8, "cm"),
        legend.title = element_text( hjust = 0.5))