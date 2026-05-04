#################################################################################################
# v004- Primary Care Physicians
# Author: KG
#Description: Ratio of population to primary care physicians.
#Data Source: Area Health Resource File/American Medical Association
#Data Download Link: https://data.hrsa.gov/data/download
#Numerator: The left side of the ratio represents the county population.
#Denominator: The right side of the ratio represents the primary care physicians corresponding to county population. Primary care physicians include practicing non-federal physicians (M.D.s and D.O.s) under age 75 specializing in general practice medicine, family medicine, internal medicine, and pediatrics.

  #v004 is the Primary Care Physicians measure. This measure was not updated for the CHR&R 2025 Annual Release, 
#but was updated in the post 2025 release rolling CHR&R updates in the Fall of 2025. This code uses the 2024-2025
#release of Area Health Resources File data, which includes provider data through 2023. The output dataset is 
#saved as v004_s2026 in measure_datasets to reflect that it was not calculated for the 2025 annual release. 

#NOTE that this measure was not updated for Connecticut with the 2024-2025 release of the AHRF. The AHRF contains 
#data for the eight former CT counties but there is no recent population data available to use as a denominator for 
#those counties for this measure. Additionally, CHR&R decided not to update CT data for any measures in the rolling 
#2025 data updates that took place after the 2025 Annual Release due to difficulties created by the geography changes.
  
##################################################################################################


library(tidyverse)

##########################NUMERATOR#####################
#bring in AHRF dataset
ahrf_raw <- read_csv("raw_data/AHRF/ahrf2024_Feb2025.csv") %>% 
  select(
    fipscode = fips_st_cnty, 
    st_name, cnty_name, 
    statecode = fips_st, 
    countycode=fips_cnty, 
    v004_numerator=phys_nf_prim_care_pc_exc_rsdt_22) %>% 
#remove Puerto Rico and USVI and guam. Leave in CT for now for national value.
  filter(!statecode %in% c("66","72","78")) #this leaves 3,158

#bring in official list of chrr counties with old CT fips to make sure there aren't other discrepancies
library(haven)
chrr_fips <-  read_sas("inputs/county_fips_with_ct_old.sas7bdat")

county_check <- full_join(ahrf_raw, chrr_fips, by ="fipscode")

#Counties below in AK and VA have missing values in the pcp dataset AND aren't listed in our master list of counties. 
##These are counties that no longer exist due to name changes or being combined with other counties. See the NOTE at the bottom of the 
#AHRF 2023-2024 Technical DocumentationCSV_Feb2025 and this webpage from the Census 
#for more details: https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html#list-tab-957819518

#Delete the old/incorrect counties (but retain CT counties with missing data).

ahrf_correct_counties <- county_check %>% 
  filter(!fipscode %in% c("02201", "02232", "02261", "02280", "51515", "51560"))

#calcuate state values by summing county values
state_values <- ahrf_correct_counties %>% 
  group_by(statecode.x) %>%
  summarise(
    v004_numerator = sum(v004_numerator, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
#set CT to missing because the state value is not updated for 2026  
  mutate(
    v004_numerator = if_else(statecode.x=="09", NA_real_, v004_numerator)
  )

#calculate national value by summing county values
ahrf_correct_counties %>% 
summarise(total = sum(v004_numerator, na.rm = TRUE))
#result was 253,985

state_and_nat <- state_values %>% 
  rename(statecode = statecode.x) %>% 
  mutate(countycode = "000") %>% 
  mutate(fipscode = paste0(statecode, countycode)) %>% 
add_row(
    statecode = "00",
    fipscode = "00000",
    countycode = "000",
    v004_numerator = 253985
  )
 

#clean up, merge, and create final numerator dataset
county_clean <-ahrf_correct_counties %>% 
  select(
    fipscode,
    statecode = statecode.x,
    countycode = countycode.x,
    v004_numerator
  ) %>% 
  mutate(
    v004_numerator = if_else(statecode=="09", NA_real_, v004_numerator)
  )
v004_numerator<- bind_rows(state_and_nat, county_clean)



####################DENOMINATOR##############

county_pop22 <- read_sas("inputs/vintage2022_with_ct_new.sas7bdat") %>% 
  mutate(fipscode = paste0(statecode, countycode)) %>% 
  filter(countycode != "000") %>% 
  select(fipscode, 
         statecode, 
         countycode, 
         v004_denominator = POPESTIMATE2022
         )

#calculate state population from sum of counties

state_pop <- county_pop22 %>% 
group_by(statecode) %>%
  summarise(
    v004_denominator = sum(v004_denominator, na.rm = TRUE),
  .groups = "drop"
  ) %>% 
  mutate(countycode = "000") %>% 
  mutate(fipscode = paste0(statecode, countycode)) %>% 
  #set CT to missing because the state value is not updated for 2026  
  mutate(
    v004_denominator = if_else(statecode=="09", NA_real_, v004_denominator)
  )

#calculate national population by summing county values
county_pop22 %>% 
  summarise(total = sum(v004_denominator, na.rm = TRUE))
#result was 333287557

state_and_nat_pop <- state_pop %>% 
  add_row(
    statecode = "00",
    fipscode = "00000",
    countycode = "000",
    v004_denominator = 333287557
  )

v004_denominator<- bind_rows(state_and_nat_pop, county_pop22) %>% 
#set CT county values to missing now that you calculated national population
  mutate(
  v004_denominator = if_else(statecode=="09", NA_real_, v004_denominator)
)

##########MEASURE CALC########
v004 <- full_join(v004_denominator, v004_numerator, by ="fipscode") %>% 
  select(
    fipscode,
    statecode = statecode.y,
    countycode = countycode.y,
    v004_numerator, v004_denominator
) %>% 
  mutate(v004_cilow = NA) %>% 
  mutate(v004_cihigh = NA) %>% 
  mutate(problem = NA) %>% 
  mutate(v004_rawvalue = v004_numerator/v004_denominator) %>% 
  mutate(v004_rawalternatevalue = v004_denominator/v004_numerator) %>% 
  

#v004_rawalternatevalue is the ratio (this measure is displayed as a ratio on our website).
#If a county has a population greater than 2,000 and 0 primary care providers, the countys v004
#value is set to missing. Because of errors with division by 0 for the rawalternatevalue, if a county pop 
#is less than 2000 and has 0 primary care providers, the rawalternatevalue is missing. On the website, the
#measure value will display as a ratio that looks like denominator:0. */ 
  mutate(
    v004_rawvalue = if_else(
      v004_numerator == 0 & v004_denominator > 2000,
      NA_real_,
      v004_rawvalue
    )
  ) %>% 
  mutate(
    v004_rawalternatevalue = if_else(
      v004_numerator ==0,
      NA_real_,
      v004_rawalternatevalue)) %>% 
#create a flag for CT counties
  mutate(
    v004_flag_ct = if_else(
    statecode =="09",
    "U",
    NA_character_
  ))


#sanity check
v004 %>%
  summarise(
    n = sum(!is.na(v004_rawvalue)),
    mean = mean(v004_rawvalue, na.rm = TRUE),
    sd = sd(v004_rawvalue, na.rm = TRUE),
    min = min(v004_rawvalue, na.rm = TRUE),
    q1 = quantile(v004_rawvalue, 0.25, na.rm = TRUE),
    median = median(v004_rawvalue, na.rm = TRUE),
    q3 = quantile(v004_rawvalue, 0.75, na.rm = TRUE),
    max = max(v004_rawvalue, na.rm = TRUE)
  )

v004 %>%
  summarise(
    n = sum(!is.na(v004_denominator)),
    mean = mean(v004_denominator, na.rm = TRUE),
    sd = sd(v004_denominator, na.rm = TRUE),
    min = min(v004_denominator, na.rm = TRUE),
    q1 = quantile(v004_denominator, 0.25, na.rm = TRUE),
    median = median(v004_denominator, na.rm = TRUE),
    q3 = quantile(v004_denominator, 0.75, na.rm = TRUE),
    max = max(v004_denominator, na.rm = TRUE)
  )

v004 %>%
  summarise(
    n = sum(!is.na(v004_numerator)),
    mean = mean(v004_numerator, na.rm = TRUE),
    sd = sd(v004_numerator, na.rm = TRUE),
    min = min(v004_numerator, na.rm = TRUE),
    q1 = quantile(v004_numerator, 0.25, na.rm = TRUE),
    median = median(v004_numerator, na.rm = TRUE),
    q3 = quantile(v004_numerator, 0.75, na.rm = TRUE),
    max = max(v004_numerator, na.rm = TRUE)
  )

#saving csv
write.csv(v004, "measure_datasets/v004_r2026.csv",row.names = FALSE)
