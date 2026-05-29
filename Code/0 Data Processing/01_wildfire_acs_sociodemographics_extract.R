# Title: Extracting American Community Survey sociodemographic characteristics by census tract
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
acs_vars_2006 <- load_variables(2008, dataset = "acs5", cache = TRUE)
acs_vars_2007 <- load_variables(2009, dataset = "acs5", cache = TRUE)
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


# for years 2007
acs_years <- c(2009)

for(j in 1:length(acs_years)) {
  
  acs_year_i <- acs_years[j]
  
  real_year <- acs_year_i - 2
  full_acs_year <- NULL
  for(i in 1:length(unique_state_fips)) {
    
    temp_acs <- get_acs(
      geography = "county",
      year = acs_year_i,
      state = unique_state_fips[i],
      variables = 
        c(
          ## race/ethnicity
          "totalpop" = "B01001_001",
          "nhWhite" = "B03002_003",
          "nhBlack" = "B03002_004",
          "nhAIAN" = "B03002_005",
          "nhAsian" = "B03002_006",
          "nhNHPI" = "B03002_007",
          "hispanic" = "B03002_012",
          
          ## unemployment 
          "af_male19" = "B23001_005",
          "unemployed_male19" = "B23001_008",
          "nilf_male19" = "B23001_009",
          "af_male21" = "B23001_012",
          "unemployed_male21" = "B23001_015",
          "nilf_male21" = "B23001_016",
          "af_male24" = "B23001_019",
          "unemployed_male24" = "B23001_022",
          "nilf_male24" = "B23001_023",
          "af_male29" = "B23001_026",
          "unemployed_male29" = "B23001_029",
          "nilf_male29" = "B23001_030",
          "af_male34" = "B23001_033",
          "unemployed_male34" = "B23001_036",
          "nilf_male34" = "B23001_037",
          "af_male44" = "B23001_040",
          "unemployed_male44" = "B23001_043",
          "nilf_male44" = "B23001_044",
          "af_male54" = "B23001_047",
          "unemployed_male54" = "B23001_050",
          "nilf_male54" = "B23001_051",
          "af_male59" = "B23001_054",
          "unemployed_male59" = "B23001_057",
          "nilf_male59" = "B23001_058",
          "af_male61" = "B23001_061",
          "unemployed_male61" = "B23001_064",
          "nilf_male61" = "B23001_065",
          "af_male64" = "B23001_068",
          "unemployed_male64" = "B23001_071",
          "nilf_male64" = "B23001_072",
          "unemployed_male69" = "B23001_076",
          "nilf_male69" = "B23001_077",
          "unemployed_male74" = "B23001_081",
          "nilf_male74" = "B23001_082",
          "unemployed_male75plus" = "B23001_086",
          "nilf_male75plus" = "B23001_087",
          
          "af_female19" = "B23001_091",
          "unemployed_female19" = "B23001_094",
          "nilf_female19" = "B23001_095",
          "af_female21" = "B23001_098",
          "unemployed_female21" = "B23001_101",
          "nilf_female21" = "B23001_102",
          "af_female24" = "B23001_105",
          "unemployed_female24" = "B23001_108",
          "nilf_female24" = "B23001_109",
          "af_female29" = "B23001_112",
          "unemployed_female29" = "B23001_115",
          "nilf_female29" = "B23001_116",
          "af_female34" = "B23001_119",
          "unemployed_female34" = "B23001_122",
          "nilf_female34" = "B23001_123",
          "af_female44" = "B23001_126",
          "unemployed_female44" = "B23001_129",
          "nilf_female44" = "B23001_130",
          "af_female54" = "B23001_133",
          "unemployed_female54" = "B23001_136",
          "nilf_female54" = "B23001_137",
          "af_female59" = "B23001_140",
          "unemployed_female59" = "B23001_143",
          "nilf_female59" = "B23001_144",
          "af_female61" = "B23001_147",
          "unemployed_female61" = "B23001_150",
          "nilf_female61" = "B23001_151",
          "af_female64" = "B23001_154",
          "unemployed_female64" = "B23001_157",
          "nilf_female64" = "B23001_158",
          "unemployed_female69" = "B23001_162",
          "nilf_female69" = "B23001_163",
          "unemployed_female74" = "B23001_167",
          "nilf_female74" = "B23001_168",
          "unemployed_female75plus" = "B23001_172",
          "nilf_female75plus" = "B23001_173",
          
          
          ## age below 15
          "male_0_5" = "B01001_003",
          "male_5_9" = "B01001_004",
          "male_10_14" = "B01001_005",
          "female_0_5" = "B01001_027",
          "female_5_9" = "B01001_028",
          "female_10_14" = "B01001_029",
          
          ## age above 65
          "male_65_66" = "B01001_020",
          "male_67_69" = "B01001_021",
          "male_70_74" = "B01001_022",
          "male_75_79" = "B01001_023",
          "male_80_84" = "B01001_024",
          "male_85_plus" = "B01001_025",
          "female_65_66" = "B01001_044",
          "female_67_69" = "B01001_045",
          "female_70_74" = "B01001_046",
          "female_75_79" = "B01001_047",
          "female_80_84" = "B01001_048",
          "female_85_plus" = "B01001_049"
          
        )
    )
    
    temp_acs <- temp_acs %>%
      select(-moe) %>%
      spread(variable, estimate) %>%
      group_by(GEOID) %>%
      transmute(
        totalpop = totalpop,
        # race ethnicity
        nhWhite_pct = round(nhWhite/totalpop*100, 3),
        nhBlack_pct = round(nhBlack/totalpop*100, 3),
        nhAIAN_pct = round(nhAIAN/totalpop*100, 3),
        nhAsian_pct = round(nhAsian/totalpop*100, 3),
        nhNHPI_pct = round(nhNHPI/totalpop*100, 3),
        hispanic_pct = round(hispanic/totalpop*100, 3),
        minor_pct = round((totalpop - nhWhite)/totalpop*100, 3),
        
        # unemployed
        unemployed_pct = round((af_male19 + unemployed_male19 + nilf_male19 + af_male21 + unemployed_male21 + nilf_male21 + af_male24 + unemployed_male24 + nilf_male24 + af_male29 + unemployed_male29 + nilf_male29 + af_male34 + unemployed_male34 + nilf_male34 + af_male44 + unemployed_male44 + nilf_male44 + af_male54 + unemployed_male54 + nilf_male54 + af_male59 + unemployed_male59 + nilf_male59 + af_male61 + unemployed_male61 + nilf_male61 + af_male64 + unemployed_male64 + nilf_male64 + unemployed_male69 + nilf_male69 + unemployed_male74 + nilf_male74 + unemployed_male75plus + nilf_male75plus + 
                                  af_female19 + unemployed_female19 + nilf_female19 + af_female21 + unemployed_female21 + nilf_female21 + af_female24 + unemployed_female24 + nilf_female24 + af_female29 + unemployed_female29 + nilf_female29 + af_female34 + unemployed_female34 + nilf_female34 + af_female44 + unemployed_female44 + nilf_female44 + af_female54 + unemployed_female54 + nilf_female54 + af_female59 + unemployed_female59 + nilf_female59 + af_female61 + unemployed_female61 + nilf_female61 + af_female64 + unemployed_female64 + nilf_female64 + unemployed_female69 + nilf_female69 + unemployed_female74 + nilf_female74 + 
                                  unemployed_female75plus + nilf_female75plus
                                )/totalpop*100, 3),
        
        # less than 15 year old
        less15pop_pct = round((male_0_5 + male_5_9 + male_10_14 + 
                                 female_0_5 + female_5_9 + female_10_14)/totalpop*100, 3),
        
        # greater than 64 year old
        greater64pop_pct = round((male_65_66 + male_67_69 + male_70_74 + 
                                    male_75_79 + male_80_84 + male_85_plus +
                                    female_65_66 + female_67_69 + female_70_74 + 
                                    female_75_79 + female_80_84 + female_85_plus)/totalpop*100, 3)
      ) %>%
      ungroup()
    
    full_acs_year <- rbind(full_acs_year, temp_acs)
  }
  saveRDS(full_acs_year, paste0("Data/1 Input Data/ACS/acs_sociodemo_county_", real_year, ".rds"))
  print(acs_year_i)
}










# for years 2008
acs_years <- c(2010)

for(j in 1:length(acs_years)) {
  
  acs_year_i <- acs_years[j]
  
  real_year <- acs_year_i - 2
  full_acs_year <- NULL
  for(i in 1:length(unique_state_fips)) {
    
    temp_acs <- get_acs(
      geography = "county",
      year = acs_year_i,
      state = unique_state_fips[i],
      variables = 
        c(
          ## race/ethnicity
          "totalpop" = "B01001_001",
          "nhWhite" = "B03002_003",
          "nhBlack" = "B03002_004",
          "nhAIAN" = "B03002_005",
          "nhAsian" = "B03002_006",
          "nhNHPI" = "B03002_007",
          "hispanic" = "B03002_012",
          
          ## unemployment 
          "af_lessHS" = "B23006_004",
          "unemployed_civilian_lessHS" = "B23006_007",
          "nilf_lessHS" = "B23006_008",
          "af_HS" = "B23006_011",
          "unemployed_civilian_HS" = "B23006_014",
          "nilf_HS" = "B23006_015",
          "af_HS2" = "B23006_018",
          "unemployed_civilian_HS2" = "B23006_021",
          "nilf_HS2" = "B23006_022",
          "af_bachelors" = "B23006_025",
          "unemployed_civilian_bachelors" = "B23006_028",
          "nilf_bachelors" = "B23006_029",

          
          ## age below 15
          "male_0_5" = "B01001_003",
          "male_5_9" = "B01001_004",
          "male_10_14" = "B01001_005",
          "female_0_5" = "B01001_027",
          "female_5_9" = "B01001_028",
          "female_10_14" = "B01001_029",
          
          ## age above 65
          "male_65_66" = "B01001_020",
          "male_67_69" = "B01001_021",
          "male_70_74" = "B01001_022",
          "male_75_79" = "B01001_023",
          "male_80_84" = "B01001_024",
          "male_85_plus" = "B01001_025",
          "female_65_66" = "B01001_044",
          "female_67_69" = "B01001_045",
          "female_70_74" = "B01001_046",
          "female_75_79" = "B01001_047",
          "female_80_84" = "B01001_048",
          "female_85_plus" = "B01001_049"
          
        )
    )
    
    temp_acs <- temp_acs %>%
      select(-moe) %>%
      spread(variable, estimate) %>%
      group_by(GEOID) %>%
      transmute(
        totalpop = totalpop,
        # race ethnicity
        nhWhite_pct = round(nhWhite/totalpop*100, 3),
        nhBlack_pct = round(nhBlack/totalpop*100, 3),
        nhAIAN_pct = round(nhAIAN/totalpop*100, 3),
        nhAsian_pct = round(nhAsian/totalpop*100, 3),
        nhNHPI_pct = round(nhNHPI/totalpop*100, 3),
        hispanic_pct = round(hispanic/totalpop*100, 3),
        minor_pct = round((totalpop - nhWhite)/totalpop*100, 3),
        
        # unemployed
        unemployed_pct = round((af_lessHS + unemployed_civilian_lessHS + nilf_lessHS + af_lessHS + unemployed_civilian_HS + nilf_HS +
                                  af_HS2 + unemployed_civilian_HS2 + nilf_HS2 + af_bachelors + unemployed_civilian_bachelors + nilf_bachelors)/totalpop*100, 3),
        
        # less than 15 year old
        less15pop_pct = round((male_0_5 + male_5_9 + male_10_14 + 
                                 female_0_5 + female_5_9 + female_10_14)/totalpop*100, 3),
        
        # greater than 64 year old
        greater64pop_pct = round((male_65_66 + male_67_69 + male_70_74 + 
                                    male_75_79 + male_80_84 + male_85_plus +
                                    female_65_66 + female_67_69 + female_70_74 + 
                                    female_75_79 + female_80_84 + female_85_plus)/totalpop*100, 3)
      ) %>%
      ungroup()
    
    full_acs_year <- rbind(full_acs_year, temp_acs)
  }
  saveRDS(full_acs_year, paste0("Data/1 Input Data/ACS/acs_sociodemo_county_", real_year, ".rds"))
  print(acs_year_i)
}








# for years 2009 - 2019
acs_years <- c(2011:2021)

for(j in 1:length(acs_years)) {
  
  acs_year_i <- acs_years[j]
  
  real_year <- acs_year_i - 2
  full_acs_year <- NULL
  for(i in 1:length(unique_state_fips)) {
    
    temp_acs <- get_acs(
      geography = "county",
      year = acs_year_i,
      state = unique_state_fips[i],
      variables = 
        c(
          ## race/ethnicity
          "totalpop" = "B01001_001",
          "nhWhite" = "B03002_003",
          "nhBlack" = "B03002_004",
          "nhAIAN" = "B03002_005",
          "nhAsian" = "B03002_006",
          "nhNHPI" = "B03002_007",
          "hispanic" = "B03002_012",
          
          ## unemployment + not working
          "unemployed" = "B23025_005",
          "armed_forces" = "B23025_006",
          "not_in_labor_force" = "B23025_007",
          
          ## age below 15
          "male_0_5" = "B01001_003",
          "male_5_9" = "B01001_004",
          "male_10_14" = "B01001_005",
          "female_0_5" = "B01001_027",
          "female_5_9" = "B01001_028",
          "female_10_14" = "B01001_029",
          
          ## age above 65
          "male_65_66" = "B01001_020",
          "male_67_69" = "B01001_021",
          "male_70_74" = "B01001_022",
          "male_75_79" = "B01001_023",
          "male_80_84" = "B01001_024",
          "male_85_plus" = "B01001_025",
          "female_65_66" = "B01001_044",
          "female_67_69" = "B01001_045",
          "female_70_74" = "B01001_046",
          "female_75_79" = "B01001_047",
          "female_80_84" = "B01001_048",
          "female_85_plus" = "B01001_049"
          
          )
    )
    
    temp_acs <- temp_acs %>%
      select(-moe) %>%
      spread(variable, estimate) %>%
      group_by(GEOID) %>%
      transmute(
        totalpop = totalpop,
        # race ethnicity
        nhWhite_pct = round(nhWhite/totalpop*100, 3),
        nhBlack_pct = round(nhBlack/totalpop*100, 3),
        nhAIAN_pct = round(nhAIAN/totalpop*100, 3),
        nhAsian_pct = round(nhAsian/totalpop*100, 3),
        nhNHPI_pct = round(nhNHPI/totalpop*100, 3),
        hispanic_pct = round(hispanic/totalpop*100, 3),
        minor_pct = round((totalpop - nhWhite)/totalpop*100, 3),
        
        # unemployed
        unemployed_pct = round((unemployed + armed_forces + not_in_labor_force)/totalpop*100, 3),
        
        # less than 15 year old
        less15pop_pct = round((male_0_5 + male_5_9 + male_10_14 + 
                                 female_0_5 + female_5_9 + female_10_14)/totalpop*100, 3),
        
        # greater than 64 year old
        greater64pop_pct = round((male_65_66 + male_67_69 + male_70_74 + 
                                    male_75_79 + male_80_84 + male_85_plus +
                                    female_65_66 + female_67_69 + female_70_74 + 
                                    female_75_79 + female_80_84 + female_85_plus)/totalpop*100, 3)
      ) %>%
      ungroup()
    
    full_acs_year <- rbind(full_acs_year, temp_acs)
  }
  saveRDS(full_acs_year, paste0("Data/1 Input Data/ACS/acs_sociodemo_county_", real_year, ".rds"))
  print(acs_year_i)
}




