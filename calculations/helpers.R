# This script contains helper functions to support calculation of CHRR measures.
# It is imported and used across different notebooks and scripts that share similar steps.


library(tidyverse)


# input paths
ipath <- list(
  county_fips_with_ct_old = "inputs/county_fips_with_ct_old.sas7bdat",
  state_fips = "inputs/state_fips.sas7bdat"
)


#' Create parent directory of given path
#' Return back given path for convenience
mkdir <- function(p) {
  d <- dirname(p)
  if (!dir.exists(d)) {
    logger::log_debug("Creating directory {d}")
    dir.create(d, recursive = TRUE)
  }
  invisible(p)
}




#' Return back given path, downloading file if it does not exist locally
get_file <- function(path, url) {

  if (file.exists(path)) return(path)

  mkdir(path)

  # Try stock download function first
  # on Windows without mode = "wb", ZIP and XLSX files get corrupted
  download_status <- try(utils::download.file(url, path, mode = "wb"))

  # If download fails, try an alternative method
  # if (download_status != 0) { # download_status is not always reliable, can be 0 even if download filed
  if (!file.exists(path)) {
    logger::log_warn("download failed, attempting alternative method... ", url)
    req <- httr2::request(url) |>
      httr2::req_headers(
        `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:149.0) Gecko/20100101 Firefox/149.0",
      )        
    resp <- httr2::req_perform(req, path)
  }
  if (file.exists(path)) {
    logger::log_info("download success: ", url, " to ", path)
    return(path)
  } else {
    logger::log_error('Download failed.\nYou can try to manually download the file from "{url}" to "{path}"')
  }
}






#' Extract state and county FIPS codes from raw ACS columns and restrict to predefined list
standardize_fips <- function(df) {
  # Predifined list of state-county FIPS codes
  standard_fips <- bind_rows(
      haven::read_sas(ipath$county_fips_with_ct_old),
      haven::read_sas(ipath$state_fips)
    )

  selected_fips <- standard_fips %>%
    select(statecode, countycode)
  df %>%
    mutate(
      geo_level = str_sub(ucgid, 1, 3),
      statecode = recode_values(
        geo_level,
        c("010") ~ "00", # US level
        c("040", "050", "140") ~ str_sub(ucgid, 10, 11) # state and county level
      ),
      countycode = recode_values(
        geo_level,
        c("010", "040") ~ "000", # US and state level
        c("050", "140") ~ str_sub(ucgid, 12, 14) # county level
      )
    ) %>%
    right_join(selected_fips, by = c("statecode", "countycode")) %>%
    arrange(statecode, countycode)
}



#' Add a flag to the data frame for Connecticut counties
#' 'A' = data available for a CT county
#' 'U' = data unavailable for a CT county
add_flag_CT <- function(df, old = "U", new = "A") {
  CT_old_lst <- c("001", "003", "005", "007", "009", "011", "013", "015")
  CT_new_lst <- c("110", "120", "130", "140", "150", "160", "170", "180", "190")

  df %>% 
    mutate(flag_CT = case_when(
      statecode == "09" & countycode %in% CT_old_lst ~ old,
      statecode == "09" & countycode %in% CT_new_lst ~ new,
      TRUE ~ NA
    ))
}

#' Prefix standard columns with "vXXX_"
add_col_prefix <- function(df, col_prefix) {
  cols_to_rename <- c("flag_CT", "rawvalue", "numerator", "denominator", "cilow", "cihigh", "sourceflag")
  rename_with(df, \(col) paste0(col_prefix, "_", col), .cols = any_of(cols_to_rename))
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

