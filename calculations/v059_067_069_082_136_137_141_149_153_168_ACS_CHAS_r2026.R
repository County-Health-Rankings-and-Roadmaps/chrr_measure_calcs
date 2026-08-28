# This script contains helper functions to support calculation of 10 r2026 measures from ACS and CHAS.
# It is imported and used in a corresponding .qmd notebook of the same name.
# Some funcions are general purpose and can be used in other calculations.


library(tidyverse)
source("calculations/helpers.R", local = (helpers <- new.env()))


# input paths
ipath <- list(
  acs5_api_vars_ = "https://api.census.gov/data/{year}/acs/acs5/{subject}variables.json",
  acs5_api_det_ = "https://api.census.gov/data/{year}/acs/acs5?get=group({tabid})&ucgid={ucgid}&key={key}",
  acs5_api_sub_ = "https://api.census.gov/data/{year}/acs/acs5/subject?get=group({tabid})&ucgid={ucgid}&key={key}",
  chas_ = "https://www.huduser.gov/portal/datasets/cp/{years}-{geo}-csv.zip",
  r2025_measure_ = "measure_datasets/{vname}_r2025.csv"
)

# output paths
opath <- list(
  acs5_vars_ = "raw_data/ACS/{year}_vars_{tab_type}.pq",
  acs5_table_ = "raw_data/ACS/{year}/{tabid}{tract}.pq",
  chas_ = "raw_data/CHAS/{years}-{geo}-csv.zip",
  r2026_measure_ = "measure_datasets/{vname}_r2026.csv"
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
  arrow::write_parquet(df, helpers$mkdir(cache_path))
  df

}






#' Retrieve ACS-5 table from Census Data API
#' US, state and county tables are downloaded in separate calls and combined into single dataframe
#' Tracts can be optionally included by setting "tract = TRUE"
#' Dataframes are cached to local parquet file for subsequent access
get_raw_table <- function(year, tabid, tract = FALSE) {

  tab_type <- substr(tabid, 1, 1)
  if (!(tab_type %in% c("B", "C", "S"))) {
    logger::log_error("Valid tables are details (B, C) and subject (S)")
  }

  cache_path <- str_glue(opath$acs5_table_, tract = ifelse(tract, "_tract", ""))
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
  ucgid_sepc <- list(
    US = "0100000US",
    state = "pseudo(0100000US$0400000)",
    county = "pseudo(0100000US$0500000)"
  )
  if (tract) ucgid_sepc$tract <- "pseudo(0100000US$1400000)"

  # perform API call for each UCGID and then combine results into single dataframe
  df <- ucgid_sepc |>
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
    # combine US, state, county and optional tract frames
    bind_rows()
  
  logger::log_info("Saving ACS-5 {year} table {tabid} to cache {cache_path}")
  arrow::write_parquet(df, helpers$mkdir(cache_path))
  df
}

#' Retrieve CHAS raw data table from a ZIP archive
get_raw_chas <- function(year, geo = c("040", "050"), table) {
  geo <- rlang::arg_match(geo)
  years <- paste0(year - 4, "thru", year)
  chas <- helpers$get_file(str_glue(opath$chas_), str_glue(ipath$chas_))
  read_csv(unz(chas, str_glue("{geo}/{table}.csv")), show_col_types = FALSE)
}



#' Calculate a derived ACS estimate along with MOEs
#' Estimate needs to be of a ratio form (v1E + v2E + ...) / (v11E + v12E + ...)
#' Numerator and denominator are passed as expressions
#' MOE columns are identified from estimate column name stubs
#' refined_variance applies "maximum varience among zero estimates" Census recommendation to variance calculation
#' and treats controlled estimates variance (code -555555555) as zero
calc_acs_ratio <- function(data, num_expr, den_expr, refined_variance = FALSE) {
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
    
    # Extract as matrices for fast matrix math
    V_mat <- as.matrix(df[moe_vars])
    # replace controlled estimate MOE with 0 or NA
    V_mat[V_mat %in% c(-555555555)] <- ifelse(refined_variance, 0, NA)
    V_mat <- (V_mat / 1.645)^2

    # No special treatment of zero estimate variances
    if (!refined_variance) {
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


