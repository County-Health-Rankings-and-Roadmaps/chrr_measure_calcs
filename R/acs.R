library(tidyverse)


source("R/util.R", local = (util <- new.env()))

# input paths
ipath <- list(
  county_fips_with_ct_old = "inputs/county_fips_with_ct_old.sas7bdat",
  state_fips = "inputs/state_fips.sas7bdat",
  acs5_api_vars_ = "https://api.census.gov/data/{year}/acs/acs5/{subject}variables.json",
  acs5_api_det_ = "https://api.census.gov/data/{year}/acs/acs5?get=group({tabid})&ucgid={ucgid}&key={key}",
  acs5_api_sub_ = "https://api.census.gov/data/{year}/acs/acs5/subject?get=group({tabid})&ucgid={ucgid}&key={key}"
)

# output paths
opath <- list(
  acs5_vars_ = "data/raw/acs5/{year}_vars_{tab_type}.pq",
  acs5_table_ = "data/raw/acs5/{year}/{tabid}.pq"
)

#' URL to preview national table at data.census.gov
data_census_gov_url <- function(year, tabid) {
  tab_type <- substr(tabid, 1, 1)
  if (tab_type == "S") {
    url <- str_glue("https://data.census.gov/table/ACSST5Y{year}.{tabid}")
  } else {
    url <- str_glue("https://data.census.gov/table/ACSDT5Y{year}.{tabid}")
  }
  url
}

#' Retrieve ACS-5 list of variables from Census Data API
get_raw_vars <- function(year, tab_type = c("B", "C", "S")) {
  tab_type <- rlang::arg_match(tab_type)

  cache_path <- str_glue(opath$acs5_vars_)
  if (file.exists(cache_path)) {
    logger::log_info("Loading ACS-5 {year} variable definitions for table type {tab_type} from cache {cache_path}")
    return(arrow::read_parquet(cache_path))
  }

  subject <- ifelse(tab_type == "S", "subject/", "")
  url <- str_glue(ipath$acs5_api_vars_)
  resp_json <- url |>
    httr2::request() |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  df <- resp_json$variables |>
    bind_rows(.id = "variable")

  logger::log_info("Saving ACS-5 {year} variable definitions for table type {tab_type} to cache {cache_path}")
  arrow::write_parquet(df, util$mkdir(cache_path))
  df

}






#' Retrieve ACS-5 table from Census Data API
#' US, state and county tables are downloaded in separate calls and combined into single dataframe
#' Dataframes are cached to local parquet file for subsequent access
get_raw_table <- function(year, tabid) {

  tab_type <- substr(tabid, 1, 1)
  if (!(tab_type %in% c("B", "C", "S"))) {
    logger::log_error("Valid tables are details (B, C) and subject (S)")
  }

  cache_path <- str_glue(opath$acs5_table_)
  if (file.exists(cache_path)) {
    logger::log_info("Loading ACS-5 {year} table {tabid} from cache {cache_path}")
    return(arrow::read_parquet(cache_path))
  }

  key <- Sys.getenv("CENSUS_API_KEY")
  if (key == "") {
    logger::log_error("CENSUS_API_KEY environmental variable is not set.")
    stop()
  }

  # UCGID parameter specifications for US, all states and all counties
  df <- list(
    US = "0100000US",
    state = "pseudo(0100000US$0400000)",
    county = "pseudo(0100000US$0500000)"
  ) |>
    # perform API call for each UCGID and then combine results into single dataframe
    imap(\(ucgid, geo_type) {
      if (tab_type == "S") {
        url <- str_glue(ipath$acs5_api_sub_)
      } else {
        url <- str_glue(ipath$acs5_api_det_)
      }
      logger::log_info("Requesting ", geo_type, " data from API endpoint ", url)
      resp_json <- url |>
        httr2::request() |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      # response is list of lists
      # first element is list of column names
      col_names <- unlist(resp_json[[1]])
      # numeric estimate and MOE columns look like B15001_001E or S0101_C02_031M
      num_cols <- grepv(paste0("^", tabid, "_.*[EM]$"), col_names)
      # all remaining elements are lists of values for every row
      # take all non-header rows, give column names to their elements and combine into a dataframe
      resp_json[-1] |>
        map(\(row) {
          # empty cells are NULL in json list
          # set them to NA, otherwise all-null columns are dropped when bind_rows()
          row[map_lgl(row, is.null)] <- NA_character_
          setNames(row, col_names)
        }) |>
        bind_rows() |>
        relocate(GEO_ID, NAME, ucgid) |>
        # convert character values to double for numeric columns
        mutate(across(all_of(num_cols), readr::parse_double))
    }) |>
    # combine US, state and column frames
    bind_rows()
  
  logger::log_info("Saving ACS-5 {year} table {tabid} to cache {cache_path}")
  arrow::write_parquet(df, util$mkdir(cache_path))
  df
}




#' Predifined list of state-county FIPS codes
standard_fips <- bind_rows(
    haven::read_sas(ipath$county_fips_with_ct_old),
    haven::read_sas(ipath$state_fips)
  ) %>% 
  arrange(statecode, countycode)


#' Extract state and county FIPS codes from raw ACS columns and restrict to predefined list
standardize_fips <- function(df) {
  selected_fips <- standard_fips %>%
    select(statecode, countycode)
  df %>%
    mutate(
      geo_level = str_sub(ucgid, 1, 3),
      statecode = recode_values(
        geo_level,
        "010" ~ "00", # US level
        c("040", "050") ~ str_sub(ucgid, 10, 11) # state and county level
      ),
      countycode = recode_values(
        geo_level,
        c("010", "040") ~ "000", # US and state level
        c("040", "050") ~ str_sub(ucgid, 12, 14) # county level
      )
    ) %>%
    right_join(selected_fips, by = c("statecode", "countycode")) %>%
    arrange(statecode, countycode)
}



#' Add a flag to the data frame for Connecticut counties
#' 'A' = data available for a CT county
#' 'U' = data unavailable for a CT county
add_flag_CT <- function(df, vnum, old = "U", new = "A") {
  CT_old_lst <- c("001", "003", "005", "007", "009", "011", "013", "015")
  CT_new_lst <- c("110", "120", "130", "140", "150", "160", "170", "180", "190")

  df %>% 
    mutate(flag_CT = case_when(
      statecode == "09" & countycode %in% CT_old_lst ~ old,
      statecode == "09" & countycode %in% CT_new_lst ~ new,
      TRUE ~ NA
    ))
}



calc_acs_ratio <- function(data, num_expr, den_expr, zero_max = TRUE) {
  # Capture the expressions as quosures
  num_enq <- enquo(num_expr)
  den_enq <- enquo(den_expr)
  
  # Extract variable names
  num_vars <- all.vars(num_enq)
  den_vars <- all.vars(den_enq)
  
  # Internal fast vectorized helper to compute variance vector
  # Optionally with max variance rule for estimates of zero
  compute_var_vector <- function(df, vars) {
    if (length(vars) == 0) return(rep(0, nrow(df)))
    
    # Identify corresponding MOE columns
    moe_vars <- str_replace(vars, "E$", "M")
    
    # Extract as matrices for fast matrix math, replacing special values with NA
    V_mat <- as.matrix(df[moe_vars])
    V_mat[V_mat %in% c(-555555555)] <- NA
    V_mat <- (V_mat / 1.645)^2

    # No special treatment of zero estimate variances
    if (!zero_max) {
      return(rowSums(V_mat))
    }

    # Sum of variances where the estimate is NOT zero
    # (E_mat != 0) creates a matrix of TRUE/FALSE (1/0) to mask non-zeroes
    E_mat <- as.matrix(df[vars])
    nonzero_sum <- rowSums(V_mat * (E_mat != 0))
    
    # Max of variances where the estimate IS zero
    # Multiplying by (E_mat == 0) turns non-zero locations into 0 variance.
    zero_vars_mat <- V_mat * (E_mat == 0)
    
    # parallel maximum across matrix columns
    zero_max <- exec(pmax, !!!as.data.frame(zero_vars_mat))
    
    # Total variance per row
    return(nonzero_sum + zero_max)

  }

  # Calculate the variance vectors
  var_num_vec <- compute_var_vector(data, num_vars)
  var_den_vec <- compute_var_vector(data, den_vars)

  data %>%
    mutate(
      numerator = !!num_enq,
      denominator = !!den_enq,
      rawvalue = numerator / denominator,
      var_num = var_num_vec,
      var_den = var_den_vec,
      
      # Variance of a proportion, accounts for correlation between numerator and denominator
      var_prop = (var_num - rawvalue^2 * var_den) / (denominator^2),
      # Final variance: if proportion formula yields negative variance, fall back uncorrelated ratio formula
      var = if_else(
        var_prop >= 0,
        var_prop,
        (var_num + rawvalue^2 * var_den) / (denominator^2)
      ),
      # 95% margin of error and CI
      moe = sqrt(var) * 1.96,
      cilow = rawvalue - moe, 
      cihigh = rawvalue + moe
    )
}



#' Apply suppression and bounds
apply_suppression <- function(df) {
  df %>% 
    mutate(
      cilow = case_when(cilow < 0 ~ 0,
                        is.na(cihigh) ~ NA_real_,
                        TRUE ~ cilow),
      cihigh = case_when(cihigh > 1 ~  1, 
                        is.na(cilow) ~ NA_real_,
                        TRUE ~ cihigh)
    )
}

add_col_prefix <- function(df, col_prefix) {
  df %>% 
    rename(
      !!(paste0(col_prefix, "_flag_CT")) := flag_CT,
      !!(paste0(col_prefix, "_rawvalue")) := rawvalue, 
      !!(paste0(col_prefix, "_numerator")) := numerator,
      !!(paste0(col_prefix, "_denominator")) := denominator,
      !!(paste0(col_prefix, "_cilow")) := cilow,
      !!(paste0(col_prefix, "_cihigh")) := cihigh,
      !!(paste0(col_prefix, "_sourceflag")) := sourceflag
    )
}