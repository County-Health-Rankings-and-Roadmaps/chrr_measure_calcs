# description -------------------------------------------------------------

# Goal: helpers for NCHS measure calculations
# Author: GL

# Load packages -----------------------------------------------------------
library(tidyverse)
library(glue)


# code --------------------------------------------------------------------

# list of census pop estimates csv files  -------------------------------------------
#' List Census Population CSV/ZIP Files
#'
#' This function returns a tibble containing the year, file name, and a year number
#' for census population data files.  It allows you to specify whether you want the
#' file names for zipped or CSV files.
#'
#' @param zipped A logical value indicating whether to return file names for zipped
#'   files (TRUE) or CSV files (FALSE). Defaults to TRUE.
#' @return A tibble with columns 'year', 'file_name', and 'year_num'.
#' @export
#' @examples
#' list_census_pop_csv() # Returns zipped file names
#' list_census_pop_csv(zipped = FALSE) # Returns CSV file names
list_census_pop_csv <- function(zipped = TRUE) {
  
  file_extension <- ifelse(zipped, ".zip", ".csv")
  
  df <- tribble(
    ~year, ~file_name, ~year_num,
    2023, glue("cc-est2023-alldata{file_extension}"), 5,
    2022, glue("cc-est2022-all{file_extension}"),     4,
    2021, glue("cc-est2021-all{file_extension}"),     3,
    2020, glue("CC-EST2020-ALLDATA{file_extension}"), 13,
    2019, glue("cc-est2019-alldata{file_extension}"), 12,
    2018, glue("cc-est2018-alldata{file_extension}"), 11,
    2017, glue("cc-est2017-alldata{file_extension}"), 10,
    2016, glue("cc-est2016-alldata{file_extension}"), 9,
    2015, glue("cc-est2015-alldata{file_extension}"), 8
  )
  
  return(df)
}


# get Census pop estimates csv file name ----------------------------------

get_census_csv_file_info <- function(year){
  csv_lst <- list_census_pop_csv(zip = FALSE)

  f_name <- csv_lst[csv_lst$year == year, ]$file_name
  yr_num <- csv_lst[csv_lst$year == year, ]$year_num

  return(
    list('year' = year, 'file_name' = f_name,
         'year_num' = yr_num)
  )

}


# read Census pop estimates csv file --------------------------------------

read_census_pop <- function(file_name, dir = census_pop_dir){
  
  print(file_name)
  
  return(
    read_csv(file.path(dir, file_name),
             na = c("X"),
             show_col_types = FALSE
    ) %>% 
      janitor::clean_names()
  )
  
}

# function to fix CT(09) old counties for 2022 and 2023 by using data from 2021
# since no pop data for CT old counties for 2022 or 2023 
# CT counties only
fix_CT_pop <- function(df_replace, df_2021){
  df_fixed <- df_replace %>%
    # remove new CT counties
    filter(!(statecode == "09" & countycode != "000")) %>%
    # use previous year's data for Connecticut
    bind_rows(df_2021 %>%
                filter(statecode == "09" & countycode != "000") ) %>%
    arrange(statecode, countycode)

  return(df_fixed)
}



# get_census_pop_by_agecat_race for v001, v127, v147, v161 -----
#' Get Census Population Data by Age Category and Race
#'
#' This function processes census data to aggregate population counts by age
#' category, and optionally by race. It filters for a specific year, applies
#' county FIPS code corrections, assigns new age categories based on census
#' age groups, and aggregates the data at the county, state, and US levels.
#'
#' @param census_ct_estimates A dataframe containing census county estimates.
#'        Must include columns `year`, `state`, `county`, `agegrp`,
#'        `tot_pop`, and columns for race categories (e.g., `nhwa_male`,
#'        `nhwa_female`, etc.).
#' @param year_num The year for which to extract census data. Defaults to 13.
#' @param by_race A logical value indicating whether to aggregate the data by race.
#'        Defaults to `FALSE`.
#' @param long_output A logical value indicating whether to return the data in long format
#'        (race as a separate column).  Only applies when `by_race = TRUE`.
#'        Defaults to `TRUE`.
#' @param age_cat_version An integer specifying the age categorization version to use.
#'   127 results in age_cat 1-8; v001 results in age_cat 1-7; v147 results in age_cat 1-18.
#'
#' @return A dataframe containing aggregated population data. The structure
#'         depends on the `by_race` and `long_output` parameters. Columns
#'         include:
#'   - `statecode`: FIPS state code.
#'   - `countycode`: FIPS county code.
#'   - `age_cat`: Age category.
#'   - (If `by_race = FALSE`): `pop`: Total population in the age category.
#'   - (If `by_race = TRUE` and `long_output = FALSE`): Columns for each race
#'      (e.g., `nh_white`, `nh_black`, etc.) containing population counts.
#'   - (If `by_race = TRUE` and `long_output = TRUE`): `race`: Recoded race value (1-8).
#'     `pop`: Population for the given race and age category.
#'
#' @examples
#' # Assuming you have a dataframe 'census_data'
#' \dontrun{
#'   # Get population data by age category (wide format)
#'   pop_age <- get_census_pop_by_agecat_race(census_ct_estimates = census_data,
#'                                            year_num = 13, by_race = FALSE,
#'                                            long_output = FALSE)
#'
#'   # Get population data by age category and race (long format)
#'   pop_race_age <- get_census_pop_by_agecat_race(census_ct_estimates = census_data,
#'                                                 year_num = 13, by_race = TRUE,
#'                                                 long_output = TRUE)
#'   head(pop_age)
#'   head(pop_race_age)
#' }
#'
#' @import dplyr
#' @import tidyr
#' @export
get_census_pop_by_agecat_race <- function(census_ct_estimates, year_num = 13, by_race = FALSE, long_output = TRUE, age_cat_version = "v001") {
  
  # Step 1: Initial filtering and common county fixes
  cc_agegrp_filtered <- census_ct_estimates %>%
    # Choose year and filter out total population age group (agegrp 0)
    filter(year == year_num, agegrp != 0) %>%
    rename(statecode = state, countycode = county) %>%
    # Apply county code fixes
    mutate(countycode = case_when(
      statecode == '46' & countycode == '113' ~ '102', # Shannon to Oglala Lakota, SD
      statecode == '51' & countycode == '515' ~ '019', # Fairfax city to Fairfax, VA
      statecode == '02' & countycode == '270' ~ '158', # Wade Hampton to Kusilvak, AK
      statecode == '02' & countycode == '201' ~ '198', # Prince of Wales-Outer Ketchikan to Prince of Wales-Hyder, AK
      TRUE ~ countycode
    ))
  
  # Step 2: Prepare data based on the 'by_race' parameter
  if (by_race) {
    cc_agegrp_prepared <- cc_agegrp_filtered %>%
      select(statecode, countycode, agegrp,
             # Non-Hispanic race categories (male and female components)
             starts_with("nhwa_"), starts_with("nhba_"), starts_with("nhia_"),
             starts_with("nhaa_"), starts_with("nhna_"), starts_with("nhtom_"),
             # Hispanic category (male and female components)
             starts_with("h_")
      ) %>%
      mutate(
        nh_white = nhwa_male + nhwa_female,
        nh_black = nhba_male + nhba_female,
        nh_aian  = nhia_male + nhia_female,
        nh_asian = nhaa_male + nhaa_female,
        nh_nhopi = nhna_male + nhna_female,
        nh_tom   = nhtom_male + nhtom_female,
        hispanic = h_male + h_female
      ) %>%
      # Remove the original male/female breakdown columns as totals are now calculated
      select(-c(nhwa_male:h_female))
    
    # Define the columns that will be summarized (race-specific)
    cols_to_summarize <- c("nh_white", "nh_black", "nh_aian", "nh_asian", "nh_nhopi", "nh_tom", "hispanic")
    
  } else {
    # If by_race is FALSE, select only the total population column
    cc_agegrp_prepared <- cc_agegrp_filtered %>%
      select(statecode, countycode, agegrp,
             pop = tot_pop # Rename tot_pop to pop for consistency
      )
    # Define the column that will be summarized (total population)
    cols_to_summarize <- c("pop")
  }
  
  # Step 3: Aggregate to age categories
  if (age_cat_version == "v001") {
    ct_pop_agecat <- cc_agegrp_prepared %>%
      # Filter for age groups 1-15, which correspond to 0-74 years
      filter(agegrp %in% c(1:15)) %>%
      # Assign new age categories based on agegrp
      mutate(age_cat = case_when(
        agegrp %in% c(1:3) ~ 1, # 0-4, 5-9, 10-14; Note: age_cat 1 will need further refinement with NCHS birth data
        agegrp %in% c(4, 5) ~ 2, # 15-19, 19-24
        agegrp %in% c(6, 7) ~ 3, # 25-29, 30-34
        agegrp %in% c(8, 9) ~ 4, # 35-39, 40-44
        agegrp %in% c(10, 11) ~ 5, # 45-49, 50-54
        agegrp %in% c(12, 13) ~ 6, # 55-59, 60-64
        agegrp %in% c(14, 15) ~ 7, # 65-69, 70-74
        TRUE ~ NA_real_
      )) %>%
      select(-agegrp) %>% # Remove original agegrp column
      group_by(statecode, countycode, age_cat) %>%
      # Summarise the dynamically chosen columns
      summarise(across(all_of(cols_to_summarize), ~sum(., na.rm = TRUE)), .groups = "drop")
  } else if (age_cat_version == "v127") {
    ct_pop_agecat <- cc_agegrp_prepared %>%
      # Filter for age groups 1-15, which correspond to 0-74 years
      filter(agegrp %in% c(1:15)) %>%
      # Assign new age categories based on agegrp
      mutate(age_cat = case_when(
        agegrp %in% c(1) ~ 1, # 0-4; will be fixed using NCHS birth data
        agegrp %in% c(2, 3) ~ 2, # 5-9, 10-14
        agegrp %in% c(4, 5) ~ 3, # 15-19, 19-24
        agegrp %in% c(6, 7) ~ 4, # 25-29, 30-34
        agegrp %in% c(8, 9) ~ 5, # 35-39, 40-44
        agegrp %in% c(10, 11) ~ 6, # 45-49, 50-54
        agegrp %in% c(12, 13) ~ 7, # 55-59, 60-64
        agegrp %in% c(14, 15) ~ 8, # 65-69, 70-74
        TRUE ~ NA_real_
      )) %>%
      select(-agegrp) %>% # Remove original agegrp column
      group_by(statecode, countycode, age_cat) %>%
      # Summarize the dynamically chosen columns
      summarise(across(all_of(cols_to_summarize), ~sum(., na.rm = TRUE)), .groups = "drop")
  } else if(age_cat_version == "v147"){
    ct_pop_agecat <- cc_agegrp_prepared %>%
      # Filter for age groups 1-18, which correspond to 0-85+ years
      filter(agegrp %in% c(1:18)) %>%
      rename(age_cat = agegrp) %>%
      group_by(statecode, countycode, age_cat) %>%
      # Summarize the dynamically chosen columns
      summarise(across(all_of(cols_to_summarize), ~sum(., na.rm = TRUE)), .groups = "drop")
  } else if (age_cat_version == "v161") {
    ct_pop_agecat <- cc_agegrp_prepared %>%
      # Filter for age groups 1-18, which correspond to 0-85+ years
      filter(agegrp %in% c(1:18)) %>%
      # Assign new age categories based on agegrp
      mutate(age_cat = case_when(
        agegrp %in% c(1)      ~ 1,  # 0-4; will be fixed using NCHS birth data
        agegrp %in% c(2, 3)   ~ 2,  # 5-9, 10-14    
        agegrp %in% c(4, 5)   ~ 3,  # 15-19, 19-24  
        agegrp %in% c(6, 7)   ~ 4,  # 25-29, 30-34
        agegrp %in% c(8, 9)   ~ 5,  # 35-39, 40-44
        agegrp %in% c(10, 11) ~ 6,  # 45-49, 50-54
        agegrp %in% c(12, 13) ~ 7,  # 55-59, 60-64
        agegrp %in% c(14, 15) ~ 8,  # 65-69, 70-74
        agegrp %in% c(16, 17) ~ 9,  # 75-79, 80-84
        agegrp %in% c(18)     ~ 10, # 85+
        TRUE ~ NA
      )) %>% 
      select(-agegrp) %>% # Remove original agegrp column
      group_by(statecode, countycode, age_cat) %>%
      # Summarize the dynamically chosen columns
      summarise(across(all_of(cols_to_summarize), ~sum(., na.rm = TRUE)), .groups = "drop")
  }  else {
    stop("Invalid age_cat_version.  Must be 'v001', 'v127', 'v147', or 'v161'")
  }
  
  # Step 4: Calculate state-level population totals (common logic)
  st_pop_agecat <- ct_pop_agecat %>%
    group_by(statecode, age_cat) %>%
    summarise(across(all_of(cols_to_summarize), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
    mutate(countycode = "000", .after = 1) # '000' is a common FIPS code for state summary
  
  # Step 5: Calculate US-level population totals (common logic)
  us_pop_agecat <- ct_pop_agecat %>%
    group_by(age_cat) %>%
    summarise(across(all_of(cols_to_summarize), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
    mutate(statecode = "00", countycode = "000", .before = 1) # '00' for US, '000' for US summary
  
  ## Step 6: Combine county, state, and US data (common logic)
  pop_all_agecat <- bind_rows(
    us_pop_agecat,
    st_pop_agecat,
    ct_pop_agecat
  ) %>%
    arrange(statecode, countycode, age_cat)
  
  # Step 7: Apply long format transformation if requested and 'by_race' is TRUE
  if (by_race && long_output) {
    pop_all_agecat_long <- pop_all_agecat %>%
      pivot_longer(cols = all_of(cols_to_summarize), names_to = "race", values_to = "pop") %>%
      mutate(race = case_when(
        race == "nh_white" ~ 1,
        race == "nh_black" ~ 2,
        race == "nh_aian"  ~ 3,
        race == "nh_asian" ~ 4,
        race == "nh_nhopi" ~ 5,
        race == "nh_tom"   ~ 6,
        race == "hispanic" ~ 8,
        TRUE ~ NA_real_
      )) %>%
      arrange(statecode, countycode, age_cat, race)
    
    return(pop_all_agecat_long)
  } else {
    # Return wide format if not by_race or if long_output is FALSE
    return(pop_all_agecat)
  }
}


# get_census_tot_pop ------
#' Get Total Census Population
#'
#' This function extracts total population data from census county estimates
#' for a specified year. It applies county FIPS code corrections and
#' aggregates the data at the county, state, and US levels.
#'
#' @param census_ct_estimates A dataframe containing census county estimates.
#'        Must include columns `year`, `state`, `county`, `agegrp`, and
#'        `tot_pop`.
#' @param year_num The year for which to extract census data. Defaults to 13.
#'
#' @return A dataframe containing total population data, with columns:
#'   - `statecode`: FIPS state code.
#'   - `countycode`: FIPS county code.
#'   - `pop`: Total population.
#'
#' @examples
#' \dontrun{
#'   # Assuming you have a dataframe 'census_data'
#'   total_pop <- get_census_tot_pop(census_ct_estimates = census_data, year_num = 13)
#'   head(total_pop)
#' }
#'
#' @import dplyr
#' @export
get_census_tot_pop <- function(census_ct_estimates, year_num = 13, by_race = FALSE, long = TRUE) {
  
  print(glue("YEAR: {year_num}"))
  
  # county pop: census pop estimates
  ct_pop <- census_ct_estimates %>%
    # choose year, age groups
    filter(year == year_num, agegrp == 0) %>%
    # select columns and calculate race-specific populations if by_race = TRUE
    {if (by_race) {
      select(., statecode = state, countycode = county, agegrp,
             # Non-Hispanic: 6
             starts_with("nhwa_"), # Not Hispanic, White alone
             starts_with("nhba_"), # Not Hispanic, Black or African American alone
             starts_with("nhia_"), # Not Hispanic, American Indian and Alaska Native alone
             starts_with("nhaa_"), # Not Hispanic, Asian alone
             starts_with("nhna_"), # Not Hispanic, Native Hawaiian and Other Pacific Islander alone
             starts_with("nhtom_"), # NHTOM_MALE	Not Hispanic, Two or More Races male population
             # Hispanic: 1
             starts_with("h_")) %>%
        mutate(
          # 6: Non-Hispanic groups
          nh_white = nhwa_male  + nhwa_female,
          nh_black = nhba_male  + nhba_female,
          nh_aian  = nhia_male  + nhia_female,
          nh_asian = nhaa_male  + nhaa_female,
          nh_nhopi = nhna_male  + nhna_female,
          nh_tom   = nhtom_male + nhtom_female,
          # 1: Hispanic
          hispanic = h_male  + h_female
        ) %>%
        select(-c(nhwa_male:h_female))
    } else {
      select(., statecode = state, countycode = county, agegrp, pop = tot_pop)
    }} %>%
    # fix counties
    mutate(countycode = case_when(
      statecode == '46' & countycode == '113' ~ '102',
      statecode == '51' & countycode == '515' ~ '019',
      statecode == '02' & countycode == '270' ~ '158',
      statecode == '02' & countycode == '201' ~ '198',
      TRUE ~ countycode
    )) %>%
    select(-agegrp)
  
  # state pop
  st_pop <- ct_pop %>%
    group_by(statecode) %>%
    {if (by_race) {
      summarise(., across(c(nh_white:hispanic), ~sum(., na.rm = TRUE)), .groups = "drop")
    } else {
      summarise(., across(c(pop), ~sum(., na.rm = TRUE)), .groups = "drop")
    }} %>%
    mutate(countycode = "000", .after = 1)
  
  # us pop
  us_pop <- ct_pop %>%
    {if (by_race) {
      summarise(., across(c(nh_white:hispanic), ~sum(., na.rm = TRUE)), .groups = "drop")
    } else {
      summarise(., across(c(pop), ~sum(., na.rm = TRUE)), .groups = "drop")
    }} %>%
    mutate(statecode = "00", countycode = "000", .before = 1)
  
  ## county + state + us data
  pop_all <- bind_rows(
    us_pop,
    st_pop,
    ct_pop) %>%
    arrange(statecode, countycode)
  
  # long form or wide form
  if (by_race && long) {
    pop_all <- pop_all %>%
      pivot_longer(cols = c(nh_white:hispanic), names_to = "race", values_to = "pop") %>%
      mutate(race = case_when(
        race == "nh_white" ~ 1,
        race == "nh_black" ~ 2,
        race == "nh_aian"  ~ 3,
        race == "nh_asian" ~ 4,
        race == "nh_nhopi" ~ 5,
        race == "nh_tom"   ~ 6,
        race == "hispanic"  ~ 8,
        TRUE ~ NA_real_))
  }
  
  return(pop_all)
  
}

# function to read mort data ----------------------------------------------

read_mort_data <- function(file_path, header, n_max = Inf){
  
  mort <- read_fwf(file_path,
                   n_max = n_max, guess_max = 100000,
                   col_positions = fwf_positions(
                     start = header$start,
                     end = header$end,
                     col_names = header$fld_name
                   ),
                   show_col_types = FALSE
  )
  
  return(mort)
}


# function to read natality data ------------------------------------------

read_nat_data <- function(file_path, header, n_max = Inf, trim_ws = TRUE){
  
  nata_data <- read_fwf(
    file_path,
    n_max = n_max, 
    guess_max = 100000,
    trim_ws = trim_ws,
    col_positions = fwf_positions(
      start = header$start,
      end = header$end,
      col_names = header$fld_name
    ),
    show_col_types = FALSE
  ) 
  
  return(nata_data)
}


# funct to calculate CI L, U of Poisson distribution n=1:99 --------
#' Calculate Confidence Intervals for Poisson Distribution Rates
#'
#' This function calculates the lower (L) and upper (U) confidence interval
#' limits for a Poisson distribution rate, given a range of observed counts (n).
#'
#' @param n A numeric vector of observed counts (e.g., number of events).  Defaults to 1:99.
#' @param alpha The confidence level (e.g., 0.95 for a 95% confidence interval). Defaults to 0.95.
#' @return A tibble with columns 'n' (observed count), 'L' (lower confidence limit),
#'   and 'U' (upper confidence limit).
#' @export
#' @examples
#' get_pois_CI(n = 1:10, alpha = 0.95)
#' get_pois_CI(n = c(5, 10, 15), alpha = 0.99) # Different alpha
get_pois_CI <- function(n = 1:99, alpha = 0.95) {
  
  # Pre-calculate alpha values to avoid repetition
  alo <- (1 - alpha) / 2
  ahi <- (1 + alpha) / 2
  
  # Vectorize the calculations for efficiency
  L <- qgamma(alo, n) / n
  U <- qgamma(ahi, n + 1) / n
  
  df_ci <- tibble(n = n, L = L, U = U)
  
  return(df_ci)
}


# get mortality data file name --------------------------------------------

get_mort_file_name <- function(year){
  return(
    paste0('MULT', year, 'US.AllCnty.txt')
  )
  
}


# get_age_cat for v001, v127, v147, v161 ------
#' Get Age Category
#'
#' This function categorizes age based on the specified version of the age
#' categorization scheme.
#'
#' @param age A numeric vector of ages.
#' @param version A string indicating the age categorization version to use.
#'        Must be either "v001" or "v127".  Defaults to "v001".
#'
#' @return A numeric vector of age categories.
#'
#' @examples
#' # Get age categories using the v001 scheme
#' get_age_cat(age = c(0, 10, 20, 50), version = "v001")
#'
#' # Get age categories using the v127 scheme
#' get_age_cat(age = c(0, 10, 20, 50), version = "v127")
#'
#' @export
get_age_cat <- function(age, version = "v001") {
  if (version == "v001") {
    case_when(
      age == 0 ~ 0,
      age <= 14 ~ 1,
      age <= 24 ~ 2,
      age <= 34 ~ 3,
      age <= 44 ~ 4,
      age <= 54 ~ 5,
      age <= 64 ~ 6,
      age <= 74 ~ 7,
      TRUE ~ 99
    )
  } else if (version == "v127") {
    case_when(
      age == 0 ~ 0,
      age <= 4 ~ 1,
      age <= 14 ~ 2,
      age <= 24 ~ 3,
      age <= 34 ~ 4,
      age <= 44 ~ 5,
      age <= 54 ~ 6,
      age <= 64 ~ 7,
      age <= 74 ~ 8,
      TRUE ~ 99
    )
  } else if (version == "v147"){
    case_when(
      age == 0 ~ 0,
      age<=4  ~ 1,
      age<=9  ~ 2,
      age<=14 ~ 3,
      age<=19 ~ 4,
      age<=24 ~ 5,
      age<=29 ~ 6,
      age<=34 ~ 7,
      age<=39 ~ 8,
      age<=44 ~ 9,
      age<=49 ~ 10,
      age<=54 ~ 11,
      age<=59 ~ 12,
      age<=64 ~ 13,
      age<=69 ~ 14,
      age<=74 ~ 15,
      age<=79 ~ 16,
      age<=84 ~ 17,
      age>=85 ~ 18,
      TRUE ~ NA
    )
  } else if (version == "v161") {
    case_when(
      age == 0 ~ 0,
      age <= 4 ~ 1,
      age <= 14 ~ 2,
      age <= 24 ~ 3,
      age <= 34 ~ 4,
      age <= 44 ~ 5,
      age <= 54 ~ 6,
      age <= 64 ~ 7,
      age <= 74 ~ 8,
      age <= 84 ~ 9,
      age >= 85 ~ 10,
      TRUE ~ NA
    )
  } else {
    stop("Invalid version. Must be 'v001', 'v127', 'v147', or 'v161'.")
  }
}

# function to assign age weight numerator based on age category for v001, v127, v147, v161 -----------
#' Get Age Weighting Factor
#'
#' This function returns an age-specific weighting factor based on the provided
#' age category and the specified version of the weighting scheme.
#'
#' @param age_cat A numeric vector of age categories.
#' @param version A string indicating the age weighting version to use.
#'        Must be either "v001" or "v127".  Defaults to "v001".
#'
#' @return A numeric vector of age weighting factors. Returns `NA_real_` for
#'         invalid age categories.
#'
#' @examples
#' # Get age weighting factors using the v001 scheme
#' get_age_wtn(age_cat = c(0, 1, 2), version = "v001")
#'
#' # Get age weighting factors using the v127 scheme
#' get_age_wtn(age_cat = c(0, 1, 2), version = "v127")
#'
#' @export
get_age_wtn <- function(age_cat, version = "v001") {
  if (version == "v001") {
    case_when(
      age_cat == 0 ~ 3794901,  # /*under 1*/
      age_cat == 1 ~ 55168238, # /*1-14 years*/
      age_cat == 2 ~ 38076743, # /*15-24 years*/
      age_cat == 3 ~ 37233437, # /*25-34 years*/
      age_cat == 4 ~ 44659185, # /*35-44 years*/
      age_cat == 5 ~ 37030152, # /*45-54 years*/
      age_cat == 6 ~ 23961506, # /*55-64 years*/
      age_cat == 7 ~ 18135514, # /*65-74 years*/
      TRUE ~ NA_real_
    )
  } else if (version == "v127") {
    case_when(
      age_cat == 0 ~ 3794901,  # /*under 1*/
      age_cat == 1 ~ 15191619, # /*1-4 years*/
      age_cat == 2 ~ 39976619, # /*5-14 years*/
      age_cat == 3 ~ 38076743, # /*15-24 years*/
      age_cat == 4 ~ 37233437, # /*25-34 years*/
      age_cat == 5 ~ 44659185, # /*35-44 years*/
      age_cat == 6 ~ 37030152, # /*45-54 years*/
      age_cat == 7 ~ 23961506, # /*55-64 years*/
      age_cat == 8 ~ 18135514, # /*65-74 years*/
      TRUE ~ NA_real_
    )
  } else if (version == "v161") {
    case_when(
      age_cat==0 ~ 3794901, #/*under 1*/
      age_cat==1 ~ 15191619,# /*1-4 years*/
      age_cat==2 ~ 39976619,# /*5-14 years*/
      age_cat==3 ~ 38076743,# /*15-24 years*/
      age_cat==4 ~ 37233437,# /*25-34 years*/
      age_cat==5 ~ 44659185,# /*35-44 years*/
      age_cat==6 ~ 37030152,# /*45-54 years*/
      age_cat==7 ~ 23961506,# /*55-64 years*/
      age_cat==8 ~ 18135514,# /*65-74 years*/
      age_cat==9 ~ 12314793, #/*75-85 years*/
      age_cat==10 ~4259173, #/*85+ years*/
      TRUE ~ NA_real_
    )
  } else {
    stop("Invalid version. Must be 'v001', 'v127', 'v147', or 'v161'.")
  }
}


# function to calculate years of life lost for v001 -------
cal_years_lost_v001 <- function(age_cat) {
  case_when(
    age_cat == 0 ~ 74.5,
    age_cat == 1 ~ 67.5, # /*1-14 years*/
    age_cat == 2 ~ 55.5, # /*15-24 years*/
    age_cat == 3 ~ 45.5, # /*25-34 years*/
    age_cat == 4 ~ 35.5, # /*35-44 years*/
    age_cat == 5 ~ 25.5, # /*45-54 years*/
    age_cat == 6 ~ 15.5, # /*55-64 years*/
    age_cat == 7 ~ 5.5 # /*65-74 years*/
  )
}


# get_mort_data_by_agecat_race for v001, v127, v147, v161 ------
#' Get Mortality Data by Age Category (and Optionally Race)
#'
#' This function processes mortality data to aggregate deaths by age category,
#' and optionally by race as well. It filters for US states and DC, corrects
#' county FIPS codes, calculates age at death, and aggregates the data.
#'
#' @param nchs_mort A dataframe containing mortality data from the NCHS.  Must include columns
#'        `state_of_residence`, `county_of_residence`, `detail_age`,
#'        `hispanic_origin`, and `race_recode_40`.
#' @param df_fips A dataframe containing FIPS codes and state codes.  Must include
#'        `state` and `statecode` columns.
#' @param by_race A logical value indicating whether to aggregate the data by race as well.
#'        Defaults to `FALSE`.
#' @param age_cat_version A string indicating the age categorization version to use.  Must be either "v001" or "v127". Defaults to "v001".
#'
#' @return A dataframe containing aggregated mortality data, with columns:
#'   - `statecode`: FIPS state code.
#'   - `countycode`: FIPS county code.
#'   - `age_cat`: Age category (as determined by `get_age_cat()`).
#'   - `deaths`: Number of deaths in the category.
#'   - (If `by_race = TRUE`): `race`: Recoded race value.
#'
#' @examples
#' # Assuming you have dataframes 'nchs_mort_data' and 'fips_data'
#' # (replace with your actual data)
#' \dontrun{
#'   mort_age <- get_mort_data_by_agecat_race(nchs_mort = nchs_mort_data, df_fips = fips_data, by_race = FALSE, age_cat_version = "v001")
#'   mort_race_age <- get_mort_data_by_agecat_race(nchs_mort = nchs_mort_data, df_fips = fips_data, by_race = TRUE, age_cat_version = "v127")
#'
#'   head(mort_age)
#'   head(mort_race_age)
#' }
#'
#' @import dplyr
#' @export
get_mort_data_by_agecat_race <- function(nchs_mort, df_fips, by_race = FALSE, age_cat_version = "v001", icd_codes = NULL) {
  
  if(!age_cat_version %in% c('v001', 'v127', 'v147', 'v161')){
    stop("Invalid age_cat_version. Must be 'v001', 'v127', 'v147', or 'v161'.")
  }
  
  mort_1 <- nchs_mort %>%
    # include only 50 states + DC
    filter(state_of_residence %in% c(state.abb, "DC")) %>%
    rename(state = state_of_residence, countycode = county_of_residence) %>%
    # fix county FIPS codes
    mutate(countycode = case_when(
      state == 'SD' & countycode == '113' ~ '102',
      state == 'AK' & countycode == '270' ~ '158',
      state == 'VA' & countycode == '515' ~ '019',
      TRUE ~ countycode
    ))
  
  if(age_cat_version == "v161"){
    mort_1 <- mort_1 %>% 
      filter(icd_code %in% icd_codes)
  }
  
  mort_2 <- mort_1 %>%
    # remove age unknown
    filter(detail_age != 9999) %>%
    filter(detail_age != 1999) %>%
    # calculate age
    mutate(age_death = case_when(
      detail_age > 1999 ~ 0,
      detail_age > 1000 & detail_age < 2000 ~ detail_age - 1000,
      TRUE ~ NA_real_
    )) %>%
    select(-c("detail_age"))
  
  # Apply age categorization based on the specified version
  mort_2 <- mort_2 %>% 
    mutate(age_cat = get_age_cat(age_death, version = age_cat_version))
  
  if (by_race) {
    mort_2 <- mort_2 %>%
      # select(-race) %>%
      mutate(race_recode_40 = as.numeric(race_recode_40)) %>%
      mutate(
        race = case_when(
          # non-Hispanic
          hispanic_origin < 200 & race_recode_40 == 1 ~ 1, # nh_white
          hispanic_origin < 200 & race_recode_40 == 2 ~ 2, # nh_black
          hispanic_origin < 200 & race_recode_40 == 3 ~ 3, # nh_aian
          hispanic_origin < 200 & race_recode_40 %in% c(4:10)  ~ 4, # nh_Asian
          hispanic_origin < 200 & race_recode_40 %in% c(11:14) ~ 5, # nh_NHOPI
          hispanic_origin < 200 & race_recode_40 %in% c(15:40) ~ 6, # nh_two or more races
          
          # Hispanic
          hispanic_origin < 300  ~ 8, # Hispanic
          
          TRUE ~ NA_real_
        )
      )
    
    # county
    mort_ct <- mort_2 %>%
      group_by(state, countycode, age_cat, race) %>%
      summarise(deaths = n(), .groups = "drop")
    
    # state
    mort_st <- mort_2 %>%
      group_by(state, age_cat, race) %>%
      summarise(countycode = "000", deaths = n(), .groups = "drop")
    
    # us
    mort_us <- mort_2 %>%
      group_by(age_cat, race) %>%
      summarise(state = "US", countycode = "000", deaths = n(), .groups = "drop")
    
    mort_all <- bind_rows(mort_ct, mort_st, mort_us) %>%
      arrange(state, countycode, race, age_cat)
    
  } else {
    # county
    mort_ct <- mort_2 %>%
      group_by(state, countycode, age_cat) %>%
      summarise(deaths = n(), .groups = "drop")
    
    # state
    mort_st <- mort_2 %>%
      group_by(state, age_cat) %>%
      summarise(countycode = "000", deaths = n(), .groups = "drop")
    
    # us
    mort_us <- mort_2 %>%
      group_by(age_cat) %>%
      summarise(state = "US", countycode = "000", deaths = n(), .groups = "drop")
    
    mort_all <- bind_rows(mort_ct, mort_st, mort_us) %>%
      arrange(state, countycode, age_cat)
    
  }
  
  
  mort_all <- mort_all %>%
    # add statecode
    left_join(df_fips %>% select(state, statecode) %>%
                distinct(),
              by = c("state")) %>%
    ungroup() %>%
    select(-state) %>%
    select(statecode, everything()) %>%
    arrange(statecode, countycode)
  
  return(mort_all)
  
}

# get mort data filtered by icd codes for v015 ------
#' Get Mortality Data Filtered by ICD Codes, Optionally by Race/Ethnicity
#'
#' This function processes NCHS mortality data, filters it by specified ICD codes,
#' and aggregates deaths at the county, state, and US levels. It can optionally
#' recode and aggregate data by race/ethnicity categories.
#'
#' @param nchs_mort A data frame containing NCHS mortality data. Expected columns
#'   include `state_of_residence`, `county_of_residence`, `icd_code`,
#'   and optionally `hispanic_origin`, `race_recode_40` if `by_race = TRUE`.
#' @param df_fips A data frame containing FIPS codes, with at least `state`
#'   (e.g., "AL", "AK") and `statecode` (e.g., "01", "02") columns for joining.
#' @param icd_codes A character vector of ICD-10 codes to filter the mortality data by.
#' @param by_race A logical value. If `TRUE`, the function will recode and
#'   aggregate mortality data by predefined race/ethnicity categories. If `FALSE`,
#'   it will aggregate total deaths only. Defaults to `FALSE`.
#'
#' @return A data frame containing aggregated mortality data with columns:
#'   `statecode`, `countycode`, and `deaths`. If `by_race = TRUE`, an additional
#'   `race` column will be included, representing numeric race codes.
#'   The data is sorted by `statecode`, `countycode`, and `race` (if present).
#'
#' @details
#' The function applies specific county FIPS code corrections for SD, AK, and VA.
#' The `race` column, if generated, uses numeric codes:
#' 1: Non-Hispanic White
#' 2: Non-Hispanic Black
#' 3: Non-Hispanic American Indian and Alaska Native
#' 4: Non-Hispanic Asian
#' 5: Non-Hispanic Native Hawaiian and Other Pacific Islander
#' 6: Non-Hispanic Two or More Races
#' 8: Hispanic
#'
#' @examples
#' # Example: Create dummy data for demonstration
#' # In a real scenario, nchs_mort and df_fips would be loaded from your datasets.
#' nchs_mort_dummy <- tibble::tribble(
#'   ~state_of_residence, ~county_of_residence, ~icd_code, ~hispanic_origin, ~race_recode_40,
#'   "AL", "001", "X85", 100, 1,
#'   "AL", "001", "X85", 100, 2,
#'   "AL", "001", "Y00", 200, 1, # Not an homicide ICD code
#'   "AL", "005", "X85", 100, 1,
#'   "AK", "270", "X90", 100, 3, # Test AK county fix
#'   "SD", "113", "X95", 100, 1, # Test SD county fix
#'   "VA", "515", "Y00", 100, 2, # Test VA county fix
#'   "CA", "001", "X85", 250, 1, # Hispanic
#'   "CA", "001", "X85", 150, 4, # Non-Hispanic Asian
#'   "CA", "001", "X85", 100, 15 # Non-Hispanic Two or More Races
#' )
#'
#' df_fips_dummy <- tibble::tribble(
#'   ~state, ~statecode,
#'   "AL", "01",
#'   "AK", "02",
#'   "SD", "46",
#'   "VA", "51",
#'   "CA", "06",
#'   "DC", "11"
#' )
#'
#' # Example ICD codes for homicide (subset for demonstration)
#' homicide_icd_codes <- c(paste0("X", 85:99), paste0("Y", 00:09))
#'
#' # Get total mortality for specified ICD codes 
#' # total_mortality <- get_mort_filtered_by_icd(
#' #   nchs_mort = nchs_mort_dummy,
#' #   df_fips = df_fips_dummy,
#' #   icd_codes = homicide_icd_codes,
#' #   by_race = FALSE
#' # )
#'
#' # Get mortality by race for specified ICD codes 
#' # race_mortality <- get_mort_filtered_by_icd(
#' #   nchs_mort = nchs_mort_dummy,
#' #   df_fips = df_fips_dummy,
#' #   icd_codes = homicide_icd_codes,
#' #   by_race = TRUE
#' # )
#'
get_mort_filtered_by_icd <- function(nchs_mort, df_fips, icd_codes, by_race = FALSE) {
  
  # Step 1: Initial filtering and common cleaning/renaming
  mort_base <- nchs_mort %>%
    # Include only 50 states + DC
    filter(state_of_residence %in% c(state.abb, "DC")) %>%
    rename(state = state_of_residence, countycode = county_of_residence) %>%
    # Fix common county FIPS codes
    mutate(countycode = case_when(
      state == 'SD' & countycode == '113' ~ '102', # Shannon to Oglala Lakota, SD
      state == 'AK' & countycode == '270' ~ '158', # Wade Hampton to Kusilvak, AK
      state == 'VA' & countycode == '515' ~ '019', # Fairfax city to Fairfax, VA
      TRUE ~ countycode
    )) %>%
    # Filter by specified ICD codes
    filter(icd_code %in% icd_codes)
  
  # Step 2: Conditional race recoding and setting up grouping variables
  if (by_race) {
    # If by_race is TRUE, perform race recoding
    mort_processed <- mort_base %>%
      # Ensure 'race' column doesn't exist from original data if it conflicts
      select(-any_of("race")) %>%
      mutate(race_recode_40 = as.numeric(race_recode_40)) %>% # Ensure numeric
      mutate(
        race = case_when(
          # Non-Hispanic categories
          (hispanic_origin > 99 & hispanic_origin < 200) & race_recode_40 == 1 ~ 1, # nh_white
          (hispanic_origin > 99 & hispanic_origin < 200) & race_recode_40 == 2 ~ 2, # nh_black
          (hispanic_origin > 99 & hispanic_origin < 200) & race_recode_40 == 3 ~ 3, # nh_aian
          (hispanic_origin > 99 & hispanic_origin < 200) & race_recode_40 %in% c(4:10) ~ 4, # nh_Asian
          (hispanic_origin > 99 & hispanic_origin < 200) & race_recode_40 %in% c(11:14) ~ 5, # nh_NHOPI
          (hispanic_origin > 99 & hispanic_origin < 200) & race_recode_40 %in% c(15:40) ~ 6, # nh_two or more races
          # Hispanic category
          hispanic_origin < 300 ~ 8, # Hispanic
          TRUE ~ NA_real_ # For any unmapped cases, though should be covered
        )
      )
    
    # Define grouping variables for each level when by_race is TRUE
    group_vars_ct <- c("state", "countycode", "race")
    group_vars_st <- c("state", "race")
    group_vars_us <- c("race")
    
  } else {
    # If by_race is FALSE, no race recoding is needed
    mort_processed <- mort_base
    
    # Define grouping variables for each level when by_race is FALSE
    group_vars_ct <- c("state", "countycode")
    group_vars_st <- c("state")
    group_vars_us <- c() # For US total without race grouping
  }
  
  # Step 3: Calculate deaths at county, state, and US levels
  # County-level aggregation
  mort_ct <- mort_processed %>%
    group_by(!!!syms(group_vars_ct)) %>% # Use syms to pass string vector as variable names
    summarise(deaths = n(), .groups = "drop")
  
  # State-level aggregation
  mort_st <- mort_processed %>%
    group_by(!!!syms(group_vars_st)) %>%
    summarise(deaths = n(), .groups = "drop") %>%
    mutate(countycode = "000", .after = 1) # '000' is a common FIPS code for state summaries
  
  # US-level aggregation
  # Handle US grouping explicitly based on by_race
  if (by_race) {
    mort_us <- mort_processed %>%
      group_by(!!!syms(group_vars_us)) %>% # Group by race for US total
      summarise(deaths = n(), .groups = "drop") %>%
      mutate(state = "US", countycode = "000", .before = 1) # 'US' for state, '000' for US summary
  } else {
    mort_us <- mort_processed %>%
      summarise(deaths = n(), .groups = "drop") %>% # No grouping for overall US total
      mutate(state = "US", countycode = "000", .before = 1)
  }
  
  # Step 4: Combine all levels and add statecode
  mort_all <- bind_rows(mort_ct, mort_st, mort_us) %>%
    # Initial arrange to ensure consistent join behavior
    arrange(state, countycode) %>%
    # Add statecode by joining with df_fips
    left_join(df_fips %>% select(state, statecode) %>% distinct(),
              by = "state") %>%
    ungroup() %>% # Ensure data is ungrouped before final select/arrange
    select(-state) %>% # Remove the state abbreviation column
    select(statecode, everything()) # Place statecode at the beginning
  
  # Step 5: Final sorting
  if (by_race) {
    # Sort by statecode, countycode, and race if by_race is TRUE
    mort_all <- mort_all %>% arrange(statecode, countycode, race)
  } else {
    # Otherwise, sort by statecode and countycode
    mort_all <- mort_all %>% arrange(statecode, countycode)
  }
  
  return(mort_all)
}

# function to get subgroups ------
#' Get Race-Specific Data
#'
#' This function extracts data for a specific race category from a dataframe,
#' renames the `rawvalue` column to a race-specific column name, and adds
#' prefixes to other column names related to the race. The exact columns
#' renamed depend on the `version` argument.
#'
#' @param df A dataframe containing race-specific data.  Must include a `race`
#'   column and a `rawvalue` column.  Other columns depend on the `version`.
#' @param race_cat The race category to filter for (numeric). Defaults to 1.
#' @param race_group The base name to use for the renamed `rawvalue` column
#'   and as a prefix for other column names (character). Defaults to
#'   "v001_race_white".  It is important to use the right prefix for the specific output.
#' @param version A string indicating which set of columns to rename. Must be
#'   either "v001", "v127", "v147", or "v161".  Defaults to "v001".  This is important because it controls which columns will be suffixed.
#'
#' @return A dataframe containing data only for the specified `race_cat`, with
#'   the `rawvalue` column renamed to `race_group`, and other columns renamed
#'   with the `race_group` prefix.  The specific columns that are renamed with
#'   the prefix depend on the `version`.
#'   - version v001 renames "cilow", "cihigh", and "flag2" to
#'     race_group_cilow, race_group_cihigh, race_group_flag2.
#'   - version v127 renames "denominator", "numerator", "cilow", and "cihigh" to
#'     race_group_denominator, race_group_numerator, race_group_cilow, race_group_cihigh.
#'
#' @examples
#' # Example with dummy data
#' df <- data.frame(
#'   race = c(1, 2, 1, 2),
#'   rawvalue = c(10, 20, 15, 25),
#'   cilow = c(5, 10, 7, 12),
#'   cihigh = c(15, 25, 20, 30),
#'   flag2 = c("A", "B", "A", "B"),
#'   denominator = c(100, 200, 150, 250),
#'   numerator = c(10, 20, 15, 25)
#' )
#'
#' # Get race 1 data with v001 columns
#' race_data_v001 <- get_race_data(df, race_cat = 1, race_group = "v001_race_white", version = "v001")
#'
#' # Get race 1 data with v127 columns
#' race_data_v127 <- get_race_data(df, race_cat = 1, race_group = "v127_race_white", version = "v127")
#'
#' print(race_data_v001)
#' print(race_data_v127)
#'
#' @import dplyr
#' @export
get_race_data <- function(df, race_cat = 1, race_group = "white", version = "v001") {
  
  race_grp <- paste0(version, "_race_", race_group)
  df <- df %>%
    filter(race == race_cat) %>%
    select(-race) %>%
    rename({{ race_grp }} := rawvalue)
  
  if (version == "v001") {
    df <- df %>%
      rename(flag2 = flag2_cb) %>%
      rename_with(.fn = ~ paste0(version, "_race_", race_group, "_", .x), 
                  .cols = c("cilow", "cihigh", "flag2"))
  } else if (version %in% c("v127", "v161", "v015", "v039", "v138", "v148", "v135", "v128") ) {
    df <- df %>%
      rename_with(.fn = ~ paste0(version, "_race_", race_group, "_", .x), 
                  .cols = c("denominator", "numerator", "cilow", "cihigh"))
  } else if (version %in% c("v147", "v129")) {
    df <- df %>%
      rename_with(.fn = ~ paste0(version, "_race_", race_group, "_", .x), 
                  .cols = c("cilow", "cihigh"))
  } else {
    stop("Invalid version. Must be 'v001', 'v127', 'v128', v147', 'v161', 'v015', 'v039', 'v138', 'v148', 'v135'.")
  }
  
  return(df)
}

#' Process Race Data in wide table format and Join with FIPS Codes
#'
#' This function processes race-specific data for multiple race categories using
#' `get_race_data` and then joins the resulting dataframes with FIPS codes.
#'
#' @param df A dataframe containing race-specific data.  Must include a `race`
#'   column and a `rawvalue` column.  Other columns depend on the `version`
#'   used within `get_race_data`.
#' @param fips A dataframe containing FIPS codes.  Must include `statecode`
#'   and `countycode` columns.
#' @param version A string indicating which version of `get_race_data` to use.
#'   Must be either "v001" or "v127".  Defaults to "v001". This is passed to get_race_data
#'
#' @return A dataframe containing the joined data for all specified race
#'   categories, combined with the FIPS codes. Columns like 'denominator',
#'   'numerator', 'ypll_se', and 'flag2_cb' are removed.
#'
#' @examples
#' # Example with dummy data
#' df <- data.frame(
#'   race = rep(1:8, each = 2), # Simulate 8 race categories
#'   rawvalue = rnorm(16),
#'   cilow = rnorm(16),
#'   cihigh = rnorm(16),
#'   flag2_cb = sample(c("A", "B"), 16, replace = TRUE),
#'   statecode = rep(1:2, 8),
#'   countycode = rep(1:2, 8)
#' )
#'
#' fips_data <- data.frame(
#'   statecode = 1:2,
#'   countycode = 1:2,
#'   fips_code = c("01001", "02003")
#' )
#'
#' processed_data <- process_race_data_wide(df, fips_data, version = "v001")
#' print(processed_data)
#'
#' @import dplyr
#' @import purrr
#' @export
process_race_data_wide <- function(df, fips, version = "v001") {
  
  race_cat_lst <- c(1, 2, 3, 4, 5, 6, 8)
  race_nm_lst <- c("white", "black", "aian", "asian", "nhopi", "tom", "hispanic")
  
  race_data_list <- map2(race_cat_lst, race_nm_lst,
                         ~ get_race_data(df, race_cat = .x, race_group = .y, version = version))
  
  names(race_data_list) <- race_nm_lst
  
  # Add FIPS codes to the list of dataframes to join
  data_to_join <- c(list(fips %>% select(statecode, countycode)), race_data_list)
  
  # Join all dataframes by statecode and countycode
  combined_data <- purrr::reduce(
    data_to_join,
    left_join,
    by = c("statecode", "countycode")
  )

  return(combined_data)
}



# get_infant_pop for age_cat = 0 from aggregated NCHS birth data ------
#' Get Infant Population Data
#'
#' This function reads NCHS birth data, filters it by year, and calculates
#' infant population counts at the county, state, and national level, optionally by race/ethnicity.
#'
#' @param yrs A numeric vector of years to include in the analysis.
#' @param file_path A character string specifying the path to the NCHS birth data CSV file.
#' @param by_race A logical value indicating whether to calculate infant population counts by race/ethnicity.
#'        Defaults to `FALSE`.
#'
#' @return A dataframe containing infant population data, with columns:
#'   - `statecode`: FIPS state code.
#'   - `countycode`: FIPS county code.
#'   - `age_cat`: Age category (always 0 for infants).
#'   - (If `by_race = FALSE`): `pop`: Total infant population.
#'   - (If `by_race = TRUE`): `race`: Recoded race/ethnicity value.
#'     `pop`: Infant population for the given race/ethnicity.
#'
#' @examples
#' \dontrun{
#'   # Get total infant population for 2020-2022
#'   infant_pop_total <- get_infant_pop(yrs = c(2020:2022),
#'                                      file_path = "C:/01_CHRR/data/NCHS_2023-10-04/nat2016_2022/nchs_births_2014_2022.csv",
#'                                      by_race = FALSE)
#'
#'   # Get infant population by race/ethnicity for 2020-2022
#'   infant_pop_race <- get_infant_pop(yrs = c(2020:2022),
#'                                     file_path = "C:/01_CHRR/data/NCHS_2023-10-04/nat2016_2022/nchs_births_2014_2022.csv",
#'                                     by_race = TRUE)
#'
#'   head(infant_pop_total)
#'   head(infant_pop_race)
#' }
#'
#' @import readr
#' @import dplyr
#' @export
get_infant_pop <- function(years, file_path, by_race = FALSE) {
  
  nchs_births <- read_csv(file_path) %>%
    filter(year %in% years)
  
  if (by_race) {
    infant_pop <- nchs_births %>%
      # exclude Hispanic unknown
      filter(hisp != 99) %>%
      # set race/eth groups as 6 + 1
      mutate(
        race = case_when(
          hisp == 0 & race == 1 ~ 1, # non-Hispanic, "White",
          hisp == 0 & race == 2 ~ 2, # non-Hispanic, "Black",
          hisp == 0 & race == 3 ~ 3, # non-Hispanic, "AIAN",
          hisp == 0 & race == 4 ~ 4, # non-Hispanic, "Asian",
          hisp == 0 & race == 5 ~ 5, # non-Hispanic, "NHOPI",
          hisp == 0 & race == 6 ~ 6, # non-Hispanic, "Two or More Races"
          hisp == 1  ~  8,           # Hispanic
          TRUE ~ NA_real_
        )
      ) %>%
      group_by(statecode, countycode, race) %>%
      summarise(pop = sum(births, na.rm = TRUE), age_cat = 0, .groups = "drop") %>%
      arrange(statecode, countycode, age_cat, race) %>%
      select(1:2, age_cat, race, pop)
    
  } else {
    infant_pop <- nchs_births %>%
      group_by(statecode, countycode) %>%
      summarise(pop = sum(births, na.rm = TRUE), age_cat = 0, .groups = "drop") %>%
      arrange(statecode, countycode, age_cat) %>%
      select(1:2, age_cat, pop)
  }
  
  return(infant_pop)
}


# get natality data file name --------------------------------------------

get_nat_file_name <- function(year){
  return(
    paste0('nat', year, 'us.AllCnty.txt')
  )
  
}

# icd codes ---------------------------------------------------------------

icd_codes_v135 <- function(){
  icd_code_lst <- c('U010','U011', 'U012', 'U013', 'U014', 'U015', 'U016', 'U017', 
                    'U018', 'U019', 'U02', 'U030','U039', 'V010', 'V011', 'V019', 
                    'V020', 'V021', 'V029', 'V030', 'V031', 'V039', 'V040', 'V041', 
                    'V049', 'V050', 'V051', 'V059', 
                    'V060', 'V061', 'V069', 'V090', 'V091', 'V092', 'V093', 'V099', 'V100', 'V101', 
                    'V102', 'V103', 'V104', 'V105', 'V109', 'V110', 'V111', 'V112', 'V113', 'V114', 
                    'V115', 'V119', 'V120','V121','V122','V123','V124', 'V125', 'V129', 
                    'V130','V131','V132', 'V133','V134','V135','V139', 'V140','V141','V142', 'V143',
                    'V144','V145','V149', 'V150','V151','V152', 'V153','V154','V155','V159', 'V160',
                    'V161','V162', 'V163','V164','V165','V169',
                    'V170','V171','V172', 'V173','V174','V175','V179', 'V180','V181','V182', 'V183',
                    'V184','V185','V189', 'V190','V191','V192', 'V193','V194','V195','V196','V198',
                    'V199', 'V200','V201','V202', 'V203','V204','V205','V209',
                    'V210','V211','V212', 'V213','V214','V215','V219', 'V220','V221','V222', 'V223',
                    'V224','V225','V229', 'V230','V231','V232', 'V233','V234','V235','V239', 'V240',
                    'V241','V242', 'V243','V244','V245','V249', 
                    'V250','V251','V252', 'V253','V254','V255','V259', 'V260','V261','V262', 'V263',
                    'V264','V265','V269', 'V270','V271','V272', 'V273','V274','V275','V279', 'V280',
                    'V281','V282', 'V283','V284','V285','V289', 
                    'V290','V291','V292', 'V293','V294','V295','V296', 'V298', 'V299', 'V300', 'V301', 
                    'V302', 'V303', 'V304', 'V305', 'V306', 'V307', 'V308', 'V309', 'V310', 'V311', 
                    'V312', 'V313', 'V314', 'V315', 'V316', 'V317', 'V318', 'V319',
                    'V320', 'V321', 'V322', 'V323', 'V324', 'V325', 'V326', 'V327', 'V328', 'V329', 
                    'V330', 'V331', 'V332', 'V333', 'V334', 'V335', 'V336', 'V337', 'V338', 'V339', 
                    'V340', 'V341', 'V342', 'V343', 'V344', 'V345', 'V346', 'V347', 'V348', 'V349', 
                    'V350', 'V351', 'V352', 'V353', 'V354', 'V355', 'V356', 'V357', 'V358', 'V359', 
                    'V360', 'V361', 'V362', 'V363', 'V364', 'V365', 'V366', 'V367', 'V368', 'V369', 
                    'V370', 'V371', 'V372', 'V373', 'V374', 'V375', 'V376', 'V377', 'V378', 'V379',
                    'V380', 'V381', 'V382', 'V383', 'V384', 'V385', 'V386', 'V387', 'V388', 'V389',
                    'V390', 'V391', 'V392', 'V393', 'V394', 'V395', 'V396', 'V397', 'V398', 'V399',
                    'V400', 'V401', 'V402', 'V403', 'V404', 'V405', 'V406', 'V407', 'V408', 'V409', 
                    'V410', 'V411', 'V412', 'V413', 'V414', 'V415', 'V416', 'V417', 'V418', 'V419', 
                    'V420', 'V421', 'V422', 'V423', 'V424', 'V425', 'V426', 'V427', 'V428', 'V429', 
                    'V430', 'V431', 'V432', 'V433', 'V434', 'V435', 'V436', 'V437', 'V438', 'V439', 
                    'V440', 'V441', 'V442', 'V443', 'V444', 'V445', 'V446', 'V447', 'V448', 'V449', 
                    'V450', 'V451', 'V452', 'V453', 'V454', 'V455', 'V456', 'V457', 'V458', 'V459', 
                    'V460', 'V461', 'V462', 'V463', 'V464', 'V465', 'V466', 'V467', 'V468', 'V469', 
                    'V470', 'V471', 'V472', 'V473', 'V474', 'V475', 'V476', 'V477', 'V478', 'V479',
                    'V480', 'V481', 'V482', 'V483', 'V484', 'V485', 'V486', 'V487', 'V488', 'V489', 
                    'V490', 'V491', 'V492', 'V493', 'V494', 'V495', 'V496', 'V497', 'V498', 'V499',
                    'V500', 'V501', 'V502', 'V503', 'V504', 'V505', 'V506', 'V507', 'V508', 'V509', 
                    'V510', 'V511', 'V512', 'V513', 'V514', 'V515', 'V516', 'V517', 'V518', 'V519', 
                    'V520', 'V521', 'V522', 'V523', 'V524', 'V525', 'V526', 'V527', 'V528', 'V529', 
                    'V530', 'V531', 'V532', 'V533', 'V534', 'V535', 'V536', 'V537', 'V538', 'V539', 
                    'V540', 'V541', 'V542', 'V543', 'V544', 'V545', 'V546', 'V547', 'V548', 'V549', 
                    'V550', 'V551', 'V552', 'V553', 'V554', 'V555', 'V556', 'V557', 'V558', 'V559',
                    'V560', 'V561', 'V562', 'V563', 'V564', 'V565', 'V566', 'V567', 'V568', 'V569', 
                    'V570', 'V571', 'V572', 'V573', 'V574', 'V575', 'V576', 'V577', 'V578', 'V579', 
                    'V580', 'V581', 'V582', 'V583', 'V584', 'V585', 'V586', 'V587', 'V588', 'V589',
                    'V590', 'V591', 'V592', 'V593', 'V594', 'V595', 'V596', 'V597', 'V598', 'V599',
                    'V600', 'V601', 'V602', 'V603', 'V604', 'V605', 'V606', 'V607', 'V608', 'V609', 
                    'V610', 'V611', 'V612', 'V613', 'V614', 'V615', 'V616', 'V617', 'V618', 'V619',
                    'V620', 'V621', 'V622', 'V623', 'V624', 'V625', 'V626', 'V627', 'V628', 'V629', 
                    'V630', 'V631', 'V632', 'V633', 'V634', 'V635', 'V636', 'V637', 'V638', 'V639', 
                    'V640', 'V641', 'V642', 'V643', 'V644', 'V645', 'V646', 'V647', 'V648', 'V649', 
                    'V650', 'V651', 'V652', 'V653', 'V654', 'V655', 'V656', 'V657', 'V658', 'V659', 
                    'V660', 'V661', 'V662', 'V663', 'V664', 'V665', 'V666', 'V667', 'V668', 'V669', 
                    'V670', 'V671', 'V672', 'V673', 'V674', 'V675', 'V676', 'V677', 'V678', 'V679',
                    'V680', 'V681', 'V682', 'V683', 'V684', 'V685', 'V686', 'V687', 'V688', 'V689', 
                    'V690', 'V691', 'V692', 'V693', 'V694', 'V695', 'V696', 'V697', 'V698', 'V699',
                    'V700', 'V701', 'V702', 'V703', 'V704', 'V705', 'V706', 'V707', 'V708', 'V709',
                    'V710', 'V711', 'V712', 'V713', 'V714', 'V715', 'V716', 'V717', 'V718', 'V719', 
                    'V720', 'V721', 'V722', 'V723', 'V724', 'V725', 'V726', 'V727', 'V728', 'V729', 
                    'V730', 'V731', 'V732', 'V733', 'V734', 'V735', 'V736', 'V737', 'V738', 'V739', 
                    'V740', 'V741', 'V742', 'V743', 'V744', 'V745', 'V746', 'V747', 'V748', 'V749', 
                    'V750', 'V751', 'V752', 'V753', 'V754', 'V755', 'V756', 'V757', 'V758', 'V759', 
                    'V760', 'V761', 'V762', 'V763', 'V764', 'V765', 'V766', 'V767', 'V768', 'V769',
                    'V770', 'V771', 'V772', 'V773', 'V774', 'V775', 'V776', 'V777', 'V778', 'V779', 
                    'V780', 'V781', 'V782', 'V783', 'V784', 'V785', 'V786', 'V787', 'V788', 'V789', 
                    'V790', 'V791', 'V792', 'V793', 'V794', 'V795', 'V796', 'V797', 'V798', 'V799', 
                    'V800', 'V801', 'V802', 'V803', 'V804', 'V805', 'V806', 'V807', 'V808', 'V809', 
                    'V810', 'V811', 'V812', 'V813', 'V814', 'V815', 'V816', 'V817', 'V818', 'V819', 
                    'V820', 'V821', 'V822', 'V823', 'V824', 'V825', 'V826', 'V827', 'V828', 'V829', 
                    'V830', 'V831', 'V832', 'V833', 'V834', 'V835', 'V836', 'V837', 'V838', 'V839', 
                    'V840', 'V841', 'V842', 'V843', 'V844', 'V845', 'V846', 'V847', 'V848', 'V849', 
                    'V850', 'V851', 'V852', 'V853', 'V854', 'V855', 'V856', 'V857', 'V858', 'V859', 
                    'V860', 'V861', 'V862', 'V863', 'V864', 'V865', 'V866', 'V867', 'V868', 'V869', 
                    'V870', 'V871', 'V872', 'V873', 'V874', 'V875', 'V876', 'V877', 'V878', 'V879', 
                    'V880', 'V881', 'V882', 'V883', 'V884', 'V885', 'V886', 'V887', 'V888', 'V889', 
                    'V890', 'V891', 'V892', 'V893', 'V894', 'V895', 'V896', 'V897', 'V898', 'V899', 
                    'V900', 'V901', 'V902', 'V903', 'V904', 'V905', 'V906', 'V907', 'V908', 'V909', 
                    'V910', 'V911', 'V912', 'V913', 'V914', 'V915', 'V916', 'V917', 'V918', 'V919',
                    'V920', 'V921', 'V922', 'V923', 'V924', 'V925', 'V926', 'V927', 'V928', 'V929', 
                    'V930', 'V931', 'V932', 'V933', 'V934', 'V935', 'V936', 'V937', 'V938', 'V939', 
                    'V940', 'V941', 'V942', 'V943', 'V944', 'V945', 'V946', 'V947', 'V948', 'V949',
                    'V950', 'V951', 'V952', 'V953', 'V954', 'V955', 'V956', 'V957', 'V958', 'V959', 
                    'V960', 'V961', 'V962', 'V963', 'V964', 'V965', 'V966', 'V967', 'V968', 'V969', 
                    'V970', 'V971', 'V972', 'V973', 'V974', 'V975', 'V976', 'V977', 'V978', 'V979', 
                    'V98', 'V99', 
                    'W00', 'W01', 'W02', 'W03', 'W04', 'W05', 'W06', 'W07', 'W08', 'W09', 'W10', 
                    'W11', 'W12', 'W13', 'W14', 'W15', 'W16','W17', 'W18', 'W19',
                    'W20', 'W21', 'W22', 'W23', 'W24', 'W25', 'W26', 'W27', 'W28', 'W29', 'W30', 
                    'W31', 'W32', 'W33', 'W34', 'W35', 'W36','W37', 'W38', 'W39','W40','W41', 'W42', 
                    'W43', 'W44', 'W45', 'W46', 'W47', 'W48', 'W49', 
                    'W50', 'W51', 'W52', 'W53', 'W54', 'W55', 'W56','W57', 'W58', 'W59','W60', 'W61', 
                    'W62', 'W63', 'W64', 'W65', 'W66','W67', 'W68', 'W69','W70', 'W71', 'W72', 'W73', 'W74', 
                    'W75', 'W76','W77', 'W78', 'W79','W80', 'W81', 'W82', 'W83', 'W84', 'W85', 'W86',
                    'W87', 'W88', 'W89','W90', 'W91', 'W92', 'W93', 'W94','W99',
                    'X00', 'X01', 'X02', 'X03', 'X04', 'X05', 'X06', 'X07', 'X08', 'X09',
                    'X10', 'X11', 'X12', 'X13', 'X14', 'X15', 'X16', 'X17', 'X18', 'X19','X20', 
                    'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29','X30', 'X31', 
                    'X32', 'X33', 'X340', 'X341', 'X348', 'X349', 'X35', 'X36', 'X37', 'X38', 'X39',
                    'X40', 'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49','X50', 
                    'X51', 'X52', 'X53', 'X54', 'X55', 'X56', 'X57', 'X58', 'X590', 'X599', 'X60', 
                    'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69','X70', 'X71', 
                    'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79',
                    'X80', 'X81', 'X82', 'X83', 'X84', 'X85', 'X86', 'X87', 'X88', 'X89','X90', 
                    'X91', 'X92', 'X93', 'X94', 'X95', 'X96', 'X97', 'X98', 'X99', 
                    'Y00', 'Y01', 'Y02', 'Y03', 'Y04', 'Y05', 'Y060', 'Y061', 'Y062', 'Y068', 'Y069', 
                    'Y070', 'Y071', 'Y072', 'Y073', 'Y078', 'Y079', 'Y08', 'Y09',
                    'Y10', 'Y11', 'Y12', 'Y13', 'Y14', 'Y15', 'Y16', 'Y17', 'Y18', 'Y19','Y20', 
                    'Y21', 'Y22', 'Y23', 'Y24', 'Y25', 'Y26', 'Y27', 'Y28', 'Y29','Y30', 'Y31', 
                    'Y32', 'Y33', 'Y34', 'Y350', 'Y351', 'Y352', 'Y353', 'Y354', 'Y355', 'Y356', 'Y357',
                    'Y360', 'Y361', 'Y362', 'Y363', 'Y364', 'Y365', 'Y366', 'Y367', 'Y368', 
                    'Y369', 'Y850', 'Y859', 'Y86', 'Y870','Y871', 'Y872', 'Y890', 'Y891', 'Y899')
  
  return(icd_code_lst)
}

icd_codes_v039 <- function(){
  icd_code_lst <- c('V021', 'V029', 'V031', 'V039', 'V041', 'V049', 'V092', 'V123','V124', 'V125', 'V129', 
                    'V133','V134','V135','V139', 'V143','V144','V145','V149', 'V194','V195','V196','V203','V204','V205','V209',
                    'V213','V214','V215','V219', 'V223','V224','V225','V229','V233','V234','V235','V239', 'V243','V244','V245','V249', 
                    'V253','V254','V255','V259', 'V263','V264','V265','V269', 'V273','V274','V275','V279', 'V283','V284','V285','V289', 
                    'V294','V295','V296', 'V298', 'V299', 'V304', 'V305', 'V306', 'V307', 'V308', 'V309', 'V314', 'V315', 'V316', 'V317', 'V318', 'V319',
                    'V324', 'V325', 'V326', 'V327', 'V328', 'V329',  'V334', 'V335', 'V336', 'V337', 'V338', 'V339', 'V344', 'V345', 'V346', 'V347', 'V348', 'V349', 
                    'V354', 'V355', 'V356', 'V357', 'V358', 'V359',  'V364', 'V365', 'V366', 'V367', 'V368', 'V369', 'V374', 'V375', 'V376', 'V377', 'V378', 'V379',
                    'V384', 'V385', 'V386', 'V387', 'V388', 'V389', 'V394', 'V395', 'V396', 'V397', 'V398', 'V399', 'V404', 'V405', 'V406', 'V407', 'V408', 'V409', 
                    'V414', 'V415', 'V416', 'V417', 'V418', 'V419', 'V424', 'V425', 'V426', 'V427', 'V428', 'V429', 'V434', 'V435', 'V436', 'V437', 'V438', 'V439', 
                    'V444', 'V445', 'V446', 'V447', 'V448', 'V449', 'V454', 'V455', 'V456', 'V457', 'V458', 'V459', 'V464', 'V465', 'V466', 'V467', 'V468', 'V469', 
                    'V474', 'V475', 'V476', 'V477', 'V478', 'V479', 'V484', 'V485', 'V486', 'V487', 'V488', 'V489',  'V494', 'V495', 'V496', 'V497', 'V498', 'V499',
                    'V504', 'V505', 'V506', 'V507', 'V508', 'V509',  'V514', 'V515', 'V516', 'V517', 'V518', 'V519', 'V524', 'V525', 'V526', 'V527', 'V528', 'V529', 
                    'V534', 'V535', 'V536', 'V537', 'V538', 'V539',  'V544', 'V545', 'V546', 'V547', 'V548', 'V549', 'V554', 'V555', 'V556', 'V557', 'V558', 'V559',
                    'V564', 'V565', 'V566', 'V567', 'V568', 'V569',  'V574', 'V575', 'V576', 'V577', 'V578', 'V579', 'V584', 'V585', 'V586', 'V587', 'V588', 'V589',
                    'V594', 'V595', 'V596', 'V597', 'V598', 'V599','V604', 'V605', 'V606', 'V607', 'V608', 'V609', 'V614', 'V615', 'V616', 'V617', 'V618', 'V619',
                    'V624', 'V625', 'V626', 'V627', 'V628', 'V629',  'V634', 'V635', 'V636', 'V637', 'V638', 'V639', 'V644', 'V645', 'V646', 'V647', 'V648', 'V649', 
                    'V654', 'V655', 'V656', 'V657', 'V658', 'V659',  'V664', 'V665', 'V666', 'V667', 'V668', 'V669', 'V674', 'V675', 'V676', 'V677', 'V678', 'V679',
                    'V684', 'V685', 'V686', 'V687', 'V688', 'V689', 'V694', 'V695', 'V696', 'V697', 'V698', 'V699', 'V704', 'V705', 'V706', 'V707', 'V708', 'V709',
                    'V714', 'V715', 'V716', 'V717', 'V718', 'V719', 'V724', 'V725', 'V726', 'V727', 'V728', 'V729',  'V734', 'V735', 'V736', 'V737', 'V738', 'V739', 
                    'V744', 'V745', 'V746', 'V747', 'V748', 'V749',  'V754', 'V755', 'V756', 'V757', 'V758', 'V759', 'V764', 'V765', 'V766', 'V767', 'V768', 'V769',
                    'V774', 'V775', 'V776', 'V777', 'V778', 'V779',  'V784', 'V785', 'V786', 'V787', 'V788', 'V789', 'V794', 'V795', 'V796', 'V797', 'V798', 'V799', 
                    'V803', 'V804', 'V805', 'V811', 'V821',  
                    'V830', 'V831', 'V832', 'V833', 'V840', 'V841', 'V842', 'V843', 'V850', 'V851', 'V852', 'V853', 
                    'V860', 'V861', 'V862', 'V863', 'V870', 'V871', 'V872', 'V873', 'V874', 'V875', 'V876', 'V877', 'V878',  'V892')
  
  return(icd_code_lst)
  
}



# add flag CT -------------------------------------------------------------

add_flag_CT <- function(df, vnum, old="U", new = "A"){
  CT_old_lst <- c("001", "003", "005", "007", "009", "011", "013", "015")
  CT_new_lst <- c("110", "120", "130", "140", "150", "160", "170", "180", "190")
  
  res <- df %>% 
    mutate(flag_CT = case_when(
      statecode == "09" & countycode %in% CT_old_lst ~ old,
      statecode == "09" & countycode %in% CT_new_lst ~ new,
      TRUE ~ NA
    )) %>% 
    rename(!!(paste0(vnum, "_flag_CT")) := flag_CT)
  
  return(res)
  
}


# functions for pop in CT new counties ------------------------------------

### function: update pop with new CT counties pop of 2022

update_pop_CT_new <- function(df_pop_all, num_yrs = 7, state = TRUE){
  
  CT_county <- df_pop_all %>% 
    filter(statecode == "09", countycode != "000") %>% 
    mutate(pop = pop * num_yrs) 
  
  CT_state <- CT_county %>% 
    mutate(countycode = "000") %>% 
    group_by(statecode, countycode) %>%
    summarise(pop = sum(pop), .groups = "drop")
  
  if(state){ # update CT state pop along with CT county pop
    res <- 
      bind_rows(
        df_pop_all %>% filter(statecode != "09"),
        CT_county, 
        CT_state
      )
  }else{ # update CT county pop only
    res <- 
      bind_rows(
        df_pop_all %>% filter(!(statecode == "09" & countycode != "000")),
        CT_county
      ) 
  }
  
  res <- 
    res %>% 
    arrange(statecode, countycode)
  
  return(res)
}


### function: update pop all with state pop
update_pop_us_from_state <- function(df_pop_all){
  
  res <- bind_rows(
    # sum pop from states
    df_pop_all %>%
      filter(statecode!="00", countycode == "000") %>%
      mutate(statecode = "00") %>%
      group_by(statecode, countycode) %>%
      summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop"),
    
    # remove orginal us pop
    df_pop_all %>%
      filter(statecode!="00")
  ) 
  
  return(res)
  
}

### function: update pop with new CT counties pop of 2022, subgroups


update_pop_CT_new_sub61 <- function(df_pop_all, num_yrs = 7, state = TRUE){
  
  CT_county <- df_pop_all %>% 
    filter(statecode == "09", countycode != "000") %>% 
    mutate(pop = pop * num_yrs) 
  
  CT_state <- CT_county %>% 
    mutate(countycode = "000") %>% 
    group_by(statecode, countycode, race) %>%
    summarise(pop = sum(pop), .groups = "drop")
  
  if(state){ # update CT state pop along with CT county pop
    res <- 
      bind_rows(
        df_pop_all %>% filter(statecode != "09"),
        CT_county, 
        CT_state
      )
  }else{ # update CT county pop only
    res <- 
      bind_rows(
        df_pop_all %>% filter(!(statecode == "09" & countycode != "000")),
        CT_county
      ) 
  }
  
  res <- 
    res %>% 
    arrange(statecode, countycode)
  
  return(res)
}


### function: update pop all with state pop, subgroups

update_pop_us_from_state_sub61 <- function(df_pop_all){
  
  res <- bind_rows(
    # sum pop from states
    df_pop_all %>%
      filter(statecode!="00", countycode == "000") %>%
      mutate(statecode = "00") %>%
      group_by(statecode, countycode, race) %>%
      summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop"),
    
    # remove orginal us pop
    df_pop_all %>%
      filter(statecode!="00")
  ) 
  
  return(res)
  
}


# functions for CT mortality data from CT vital records data --------------


### function: calculate CT counties from CT mortality data

get_CT_from_CT_mort <- function(df, year_lst = 2016:2022, icd_code_lst=NULL){
  
  df_2 <- df %>% 
    filter(state == "CT") %>% 
    filter(year %in% year_lst) %>% 
    mutate(statecode = "09",
           countycode = str_pad(countycode, 3, "left", "0"))
  
  if(!is.null(icd_code_lst)){
    df_2 <- df_2 %>% 
      filter(icd_code %in% icd_code_lst) 
  }
  
  CT_mort_county <-df_2 %>% 
    group_by(statecode, countycode) %>%
    summarise(deaths = n(), .groups = "drop")
  
  CT_mort_state <- CT_mort_county %>% 
    mutate(countycode = "000") %>%
    group_by(statecode, countycode) %>%
    summarise(deaths = sum(deaths), .groups = "drop")
  
  res <- bind_rows(CT_mort_county, CT_mort_state) %>% 
    arrange(statecode, countycode)
  
  return(res)
  
}


### function: update mort_all with new CT counties


update_mort_CT_new <- function(df_mort_all, df_CT_mort_new, state = TRUE){
  
  if(state){
    res <- 
      bind_rows(
        df_mort_all %>% filter(statecode != "09"),
        df_CT_mort_new
      ) 
  }else{
    res <- 
      bind_rows(
        df_mort_all %>% filter(!(statecode == "09" & countycode != "000")),
        df_CT_mort_new %>% filter(countycode != "000")
      ) 
  }
  
  res <- 
    res %>% 
    arrange(statecode, countycode)
  
  return(res)
}


### function: update mort_all with state mort

update_mort_us_from_state <- function(df_mort_all){
  
  res <- bind_rows(
    # sum pop from states
    df_mort_all %>%
      filter(statecode!="00", countycode == "000") %>%
      mutate(statecode = "00") %>%
      group_by(statecode, countycode) %>%
      summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop"),
    
    # remove orginal us pop
    df_mort_all %>%
      filter(statecode!="00")
  ) 
  
  return(res)
  
}

### function: calculate CT counties from CT mortality data, subgroups

get_CT_from_CT_mort_sub61 <- function(df, year_lst = 2016:2022, icd_code_lst=NULL){
  
  df_2 <- df %>% 
    filter(state == "CT") %>% 
    filter(year %in% year_lst) %>% 
    mutate(statecode = "09",
           countycode = str_pad(countycode, 3, "left", "0"))
  
  if(!is.null(icd_code_lst)){
    df_2 <- df_2 %>% 
      filter(icd_code %in% icd_code_lst) 
  }
  
  CT_mort_county <-df_2 %>% 
    mutate(race = case_when(
      RACE6_ETH_label == "WHITE-NH" ~ 1, # nh_white
      RACE6_ETH_label == "BLACK-NH" ~ 2, # nh_black
      RACE6_ETH_label == "AMIN-NH"  ~ 3, # nh_aian
      RACE6_ETH_label == "ASIAN-NH" ~ 4, # nh_asian
      RACE6_ETH_label == "TOM-NH"   ~ 6, # nh_tom
      RACE6_ETH_label == "HISPANIC" ~ 8, # black
      TRUE ~ NA)
    ) %>% 
    group_by(statecode, countycode, race) %>%
    summarise(deaths = n(), .groups = "drop")
  
  CT_mort_state <- CT_mort_county %>% 
    mutate(countycode = "000") %>%
    group_by(statecode, countycode, race) %>%
    summarise(deaths = sum(deaths), .groups = "drop")
  
  res <- bind_rows(CT_mort_county, CT_mort_state) %>% 
    filter(!is.na(race)) %>% 
    arrange(statecode, countycode, race)
  
  return(res)
  
}


### function: update mort_all with new CT counties, subgroups


update_mort_CT_new_sub61 <- function(df_mort_all, df_CT_mort_new, state = TRUE){
  
  if(state){
    res <- 
      bind_rows(
        df_mort_all %>% filter(statecode != "09"),
        df_CT_mort_new
      ) 
  }else{
    res <- 
      bind_rows(
        df_mort_all %>% filter(!(statecode == "09" & countycode != "000")),
        df_CT_mort_new %>% filter(countycode != "000")
      ) 
  }
  
  res <- 
    res %>% 
    arrange(statecode, countycode, race)
  
  return(res)
}


### function: update mort_all with state mort, subgroups

update_mort_us_from_state_sub61 <- function(df_mort_all){
  
  res <- bind_rows(
    # sum mort from states
    df_mort_all %>%
      filter(statecode!="00", countycode == "000") %>%
      mutate(statecode = "00") %>%
      group_by(statecode, countycode, race) %>%
      summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop"),
    
    # remove orginal us pop
    df_mort_all %>%
      filter(statecode!="00")
  ) 
  
  return(res)
  
}

# function: save calculated data as a csv file
save_data <- function(df, vnum, output_dir = "../measure_datasets/") {
  # Construct the filename. Use paste0 for efficiency.
  filename <- glue("{output_dir}{vnum}_r2025.csv")
  
  # Write the CSV file with error handling.  Use tryCatch for robust error management.
  tryCatch({
    readr::write_csv(df, filename, na = "") 
    message(paste("Data saved to:", filename)) 
  }, error = function(e) {
    stop(paste("Error writing data to CSV:", e$message))
  })
}


# end -------------------- ----------------------------------------------------



# Functions for comparing initial calculations to duplications  

#' Compare Duplicated and Initial Measure Calculations (no subgroups) 
#'
#' This function compares a duplicated version of a measure dataset 
#' to the initial version calculated and stored for a given year. It reads 
#' the initial SAS dataset from the Pdrive, merges it with the provided duplicated data 
#' (by state and county FIPS), and computes the absolute differences for 
#' key measure components: numerator, denominator, raw value, and confidence intervals.
#' It also reports the number of missing values in both datasets.
#' This function is for measures that have ci values but do not have race/other values 
#'
#' @param measure_num A character string indicating the measure (e.g., "v001")
#' @param duplicated_data A data frame containing the duplicated calculations
#' @param year An integer or string indicating the year of the data (e.g., 2026)
#'
#' @return A data frame of rows with differences greater than 0.001 in any component
#' @examples
#' compare("v001", v001, 2026)

compare <- function(measure_num, duplicated_data, year) {
  library(dplyr)
  library(haven)
  
  # Read initial calculation 
  initial <- read_sas(paste0("P:/CH-Ranking/Data/", year, "/3 Data calculated needs checking/", measure_num, ".sas7bdat"))
  
  # rename duplicated data 
  dup = duplicated_data
  
  
  # Merge main measure
  initial_dup <- merge(initial, dup, by = c("statecode", "countycode"))
  
  # Get all components (numerator, denominator, rawvalue, cilow, cihigh)
  components <- c("numerator", "denominator", "rawvalue", "cilow", "cihigh")
  
  # Initialize list for NA counts
  na_report <- list()
  
  # Loop over components to calculate absolute differences
  for (comp in components) {
    col_x <- paste0(measure_num, "_", comp, ".x")
    col_y <- paste0(measure_num, "_", comp, ".y")
    diff_col <- paste0("diff_", comp)
    
    if (col_x %in% names(initial_dup) && col_y %in% names(initial_dup)) {
      initial_dup[[diff_col]] <- abs(initial_dup[[col_x]] - initial_dup[[col_y]])
      
      na_report[[comp]] <- c(
        initial_NA = sum(is.na(initial_dup[[col_x]])),
        duplicated_NA = sum(is.na(initial_dup[[col_y]]))
      )
      
    } else {
      warning(paste("Missing expected columns:", col_x, "or", col_y))
    }
  }
  
  # Print summary
  cat("Summary of Differences:\n")
  print(sapply(initial_dup %>% select(starts_with("diff_")), summary))
  
  # Print summary of missing values
  cat("\nMissing Value Report:\n")
  na_df <- do.call(rbind, na_report) %>% as.data.frame()
  print(na_df)
  
  # Filter rows with any meaningful differences
  temp <- initial_dup %>% filter(if_any(starts_with("diff_"), ~ abs(.) > 0.001))
  
  cat(paste("\n", nrow(temp), "rows with significant mismatches found.\n"))
  
  return(temp)
  
}

###################################################################################
#' Compare Duplicated and Initial Measure Calculations (with race subgroups)
#'
#' This function compares a duplicated version of a measure dataset 
#' to the initial version calculated and stored for a given year. It reads 
#' the initial SAS dataset from the P drive, merges it with the provided duplicated data 
#' (by state and county FIPS), and computes the absolute differences for 
#' key measure components: numerator, denominator, raw value, and confidence intervals.
#' 
#' In addition to the main components, this function also compares values for 
#' race subgroups where present. Race subgroup columns are expected to follow the 
#' naming convention: <measure_num>_race_<group>, <measure_num>_race_<group>_cilow, 
#' and <measure_num>_race_<group>_cihigh, where <group> includes 
#' "white", "black", "aian", "asian", "nhopi", and "tom".
#' 
#' It also reports the number of missing values in both datasets for each component.
#'
#' @param measure_num A character string indicating the measure (e.g., "v001")
#' @param duplicated_data A data frame containing the duplicated calculations
#' @param year An integer or string indicating the year of the data (e.g., 2026)
#'
#' @return A data frame of rows with differences greater than 0.001 in any component
#' @examples
#' compare_subgroup("v001", v001, 2026)

compare_subgroup <- function(measure_num, duplicated_data, year) {
  library(dplyr)
  library(haven)
  
  # Read initial calculation 
  initial <- read_sas(paste0("P:/CH-Ranking/Data/", year, "/3 Data calculated needs checking/", measure_num, "_otherdata.sas7bdat"))
  
  # Rename duplicated data
  dup <- duplicated_data
  
  # Merge datasets
  initial_dup <- merge(initial, dup, by = c("statecode", "countycode"))
  
  # Main components to check
  components <- c("numerator", "denominator", "rawvalue", "cilow", "cihigh")
  
  # Race subgroup suffixes
  race_groups <- c("white", "black", "aian", "asian", "nhopi", "tom")
  race_suffixes <- c("", "_cilow", "_cihigh")
  race_components <- unlist(lapply(race_groups, function(rg) paste0("_race_", rg, race_suffixes)))
  
  # Full list of all components to check
  all_components <- c(components, race_components)
  
  # Initialize list for NA counts
  na_report <- list()
  
  # Loop over all components to calculate absolute differences
  for (comp in all_components) {
    col_x <- paste0(measure_num, comp, ".x")
    col_y <- paste0(measure_num, comp, ".y")
    diff_col <- paste0("diff", comp)
    
    if (col_x %in% names(initial_dup) && col_y %in% names(initial_dup)) {
      initial_dup[[diff_col]] <- abs(initial_dup[[col_x]] - initial_dup[[col_y]])
      
      na_report[[comp]] <- c(
        initial_NA = sum(is.na(initial_dup[[col_x]])),
        duplicated_NA = sum(is.na(initial_dup[[col_y]]))
      )
    } else {
      warning(paste("Missing expected columns:", col_x, "or", col_y))
    }
  }
  
  # Print summary of differences
  cat("Summary of Differences:\n")
  print(sapply(initial_dup %>% select(starts_with("diff")), summary))
  
  # Print summary of missing values
  cat("\nMissing Value Report:\n")
  na_df <- do.call(rbind, na_report) %>% as.data.frame()
  print(na_df)
  
  # Filter rows with any significant differences
  temp <- initial_dup %>% filter(if_any(starts_with("diff"), ~ abs(.) > 0.001))
  
  cat(paste("\n", nrow(temp), "rows with significant mismatches found.\n"))
  
  return(temp)
}
