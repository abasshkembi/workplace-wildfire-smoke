# workplace-wildfire-smoke
Characterizing wildfire smoke exposure for outdoor workers and the implications on environmental justice and mortality

--------------------------------------------------------------------------------

*Code for*

**Abas Shkembi**, Sara D. Adar, Richard L. Neitzel, Marissa L. Childs (2026). Workplace exposures may mask wildfire smoke-related exposure inequities and mortality. *Environmental Science & Technology Letters*. https://doi.org/10.1021/acs.estlett.6c00273

**Contact:** Abas Shkembi (ashkembi@umich.edu)

### Description

This repository stores the dataset and code that can be used to replicate analyses for the following paper, published at *ES&T Letters*: https://doi.org/10.1021/acs.estlett.6c00273

Download the estimates yourself through Zenodo: https://doi.org/10.5281/zenodo.20435507

### Code/

The R scripts in this repository can be used to replicate our study findings. The scripts document the steps for using for processing the necessary data inputs, exposure estimation of workplace wildfire smoke exposure, and analyzing the outputs.

#### 0 Data Processing/

  * **00_wildfire_acs_occupation_extract.R** - extracts employment counts by major occupational group and county from US American Community Survey (ACS)
  * **01_wildfire_acs_sociodemographics_extract.R** - extracts sociodemographic characteristics by county from ACS
  * **02_wildfire_acs_occupation_processing.R** - processes ACS employment count data by occupational group
  * **03_wildfire_bls_oews_processing.R** - processes Bureau of Labor Statistics (BLS) Occupational Employment and Wage Statistics (OEWS) employment counts by metropolitan/nonmetropolitan area
  * **04_wildfire_onet_processing.R** - processes Occupational Information Network (O*NET) data on occupational characteristics that influence wildfire smoke exposure

#### 1 Workplace Exposures/

  * **10_wildfire_exposure_assessment.R** - estimates daily, county-level prevalence of potentially hazardous wildfire smoke exposure
  * **11_wildfire_county_poststratification.R** - poststratification to estimate number of workers exposed by county-year

#### 2 Analysis/

  * **20_exposure_descriptives.R** - descriptive statistics of workplace wildfire smoke exposure
  * **21_exposure_inequities.R** - assessing whether racial and ethnic minority communities have higher workplace heat exposure than ambient exposure
  * **22_mortality_models.R** - assessing whether ambient exposure-mortality relationship is modeified by workplace exposures

### Data/

#### 1 Input Data/

  * **ACS/acs_occ_county_{YEAR}.rds** - generated from script `00_wildfire_acs_occupation_extract.R`; not stored on repo due to large file size
  * **ACS/acs_sociodemo_county_{YEAR}.rds** - generated from script `01_wildfire_acs_sociodemographics_extract.R`; not stored on repo due to large file size
  * **ACS/SOC_ACS_crosswalk.xlsx** - provides a crosswalk between SOC codes and ACS codes for occupational groups
  
  * **BLS OEWS/2019/MSA_M2019_dl.xlsx** - provides employment counts by detailed SOC code and metropolitan area from Bureau of Labor Statistics (BLS) Occupational Employment and Wage Statistics OEWS
  * **BLS OEWS/2019/BOS_M2019_dl.xlsx** - provides employment counts by detailed SOC code and nonmetropolitan area from BLS OEWS
  * **BLS OEWS/county_mnm_cross_2019.RData** - provides crosswalk between county codes and metro/nonmetro codes
  * **BLS OEWS/divisions_2018.xlsx** - provides crosswalk of changes between metropolitan areas over the years
  * **BLS OEWS/nonmet_2018.xlsx** - provides crosswalk of changes between nonmetropolitan areas over the years
  * **BLS OEWS/soc_2010_to_2018_crosswalk.xlsx** - provides crosswalk between 2010 and 2018 SOC codes since BLS OEWS data uses 2018 SOC codes

  * **Mortality/Allcause/allcause_county_yearly_age{AGE1}to{AGE2}_{YEAR1}_{YEAR2}.xlsx** - county-year, all-cause mortality counts
 
  * **ONET/db_24_1_excel/Work Context.xlsx** - Occupational Information Network (O*NET) data on occupational characteristics that influence susceptibility to heat

  * **Precipitation/noaa_precip_{YEAR}.csv** - total precipitation by county-year

  * **Tract Centroids/census_tract_centroid_2010.rds** - latitude/longitude centroids for each 2010 census tract in the US
  
  * **Wildfire/smokePM2pt5_predictions_daily_county_20060101-20201231.{rds/csv}** - daily, smoke PM2.5 levels, downloaded from https://doi.org/10.1021/acs.est.2c02934

#### 2 Processeed Data/

  * **ACS/process_acs_occ_county_{YEAR}.rds** - generated from script `02_wildfire_acs_occupation_processing.R`
  
  * **BLS OEWS/process_oews_occ_county_2019.rds** - generated from script `03_wildfire_bls_oews_processing.R`

  * **ONET/process_onet_wildfire_2019.rds** - generated from script `04_wildfire_onet_processing.R`

#### 3 Generated Data/

  * **County Proportions/daily_wildfire_prop_occ_county_{YEAR}.rds** - county proportion of workers exposed to wildfire smoke every day from 2006-2019; generated from script `10_wildfire_exposure_assessment.R`

  * **County Exposure Estimates/wildfire_exp_county_year_2006_2019.rds** - Poststratified county proportion of workers exposed to wildfire smoke from 2006-2019; generated from script `11_wildfire_county_poststratification.R`
