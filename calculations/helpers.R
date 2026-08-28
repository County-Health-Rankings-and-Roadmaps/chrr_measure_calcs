# This script contains helper functions to support calculation of CHRR measures.
# It is imported and used across different notebooks and scripts that share similar steps.


library(tidyverse)


# input paths
ipath <- list(
  county_fips_with_ct_old = "inputs/county_fips_with_ct_old.sas7bdat",
  state_fips = "inputs/state_fips.sas7bdat",
  s3_endpoint = "s3.wisc.edu",
  s3_region =  "web",
  s3_bucket = "countyhealthrankings",
  s3_url = "https://{ipath$s3_region}.{ipath$s3_endpoint}/{ipath$s3_bucket}/{object_path}"
)



#' Configure write channels for the "logger" messages
#' stderr is seen in console output, stdout in rendered notebooks
logger_config <- function(stderr = TRUE, stdout = FALSE) {
  if (stderr && !stdout) {
    logger::log_appender(logger::appender_stderr)
  } else if (!stderr && stdout) {
    logger::log_appender(logger::appender_stdout)
  } else if (stderr && stdout) {
    logger::log_appender(function(lines) {
      logger::appender_stdout(lines)
      logger::appender_stderr(lines)
    })
  }
}



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


#' Custom downloader function
#' Attempts an alternative method if default download.file() fails
download_file <- function(url, path) {
  logger::log_info("Downloading {url} to {path}")
  # Try stock download function first
  # on Windows without mode = "wb", ZIP and XLSX files get corrupted
  download_status <- try(utils::download.file(url, path, mode = "wb"))

  # If download fails, try an alternative method
  # download_status is not always reliable, can be 0 even if download filed
  if (!file.exists(path)) {
    logger::log_warn("Download failed, attempting alternative method ", url)
    req <- httr2::request(url) |>
      httr2::req_headers(
        `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:149.0) Gecko/20100101 Firefox/149.0",
      )        
    resp <- httr2::req_perform(req, path)
  }

  if (file.exists(path)) {
    logger::log_info("Downloaded {path}")
    return(path)
  } else {
    logger::log_error('Download failed.\nYou can try to manually download the file from "{url}" to "{path}"')
    stop()
  }

}



#' Return back given path, downloading file if it does not exist locally
#' If local file does not exist and `s3_read == TRUE`, try downloading file from S3 before hitting source URL
#' If `s3_write` then file is saved to S3 after download
get_file <- function(path, url, s3_read = FALSE, s3_write = FALSE) {

  if (file.exists(path)) {
    logger::log_info("Found local file at ", path)
    # write backup file to S3
    if (s3_write) save_to_s3(path)
    return(path)
  }

  logger::log_info("Local file not found, proceeding to download ", path)

  mkdir(path)

  # attempt to download backup file from S3
  if (s3_read && get_s3_file(path, public = TRUE)) return(path)
  
  download_file(url, path)

  # write backup file to S3
  if (s3_write) save_to_s3(path)
  
  path
}



#' Write object to disk and cloud storage
#' `path` must be relative to working directory
save_to_file <- function(object, path) {
  if (file.exists(path)) logger::log_info("Overwriting local file ", path)
  if (str_ends(path, "\\.csv")) {
    write_csv(object, path, na = "")
  } else if (str_ends(path, "\\.(pq|parquet)")) {
    arrow::write_parquet(object, path)
  }
  logger::log_info("Saved local file ", path)
}


#' Download file from S3
#' Returns TRUE if file was successfully downloaded, otherwise FALSE
#' If file is `public`, it will be downloaded from HTTP url
#' Otherwise through authenticated S3 request
get_s3_file <- function(path, public = FALSE) {

  if (public) {
    # publicly available file: download from HTTP url
    url <- str_glue(ipath$s3_url, object_path = path)
    download_file(url, path)
    return(file.exists(path))
  } else {
    # file not public, access with AWS credentials
    if (any(Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")) == "")) {
      logger::log_error("AWS_ACCESS_KEY_ID or AWS_SECRET_ACCESS_KEY env variable not set, aborting S3 upload.")
      return(FALSE)
    }

    # do not overwrite user space env vars
    withr::with_envvar(
      new = c("AWS_S3_ENDPOINT" = ipath$s3_endpoint, "AWS_DEFAULT_REGION" = ipath$s3_region),
      {
        if (suppressMessages(aws.s3::object_exists(path, ipath$s3_bucket))) {
          if (file.exists(path)) logger::log_info("Overwriting local file ", path)
          aws.s3::save_object(object = path, bucket = ipath$s3_bucket, file = path, show_progress = TRUE)
          if (file.exists(path)) {
            logger::log_info("Downloaded S3 file ", path)
            return(TRUE)
          }
        } else {
          logger::log_info("S3 file not found ", path)
          return(FALSE)
        }
      }
    )
  }

  FALSE
}



#' Upload file to S3
save_to_s3 <- function(path) {
  stopifnot(file.exists(path))
  if (any(Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")) == "")) {
    logger::log_error("AWS_ACCESS_KEY_ID or AWS_SECRET_ACCESS_KEY env variable not set, aborting S3 upload.")
    stop()
  }

  # do not overwrite user space env vars
  withr::with_envvar(
    new = c("AWS_S3_ENDPOINT" = ipath$s3_endpoint, "AWS_DEFAULT_REGION" = ipath$s3_region),
    {
      if (suppressMessages(aws.s3::object_exists(path, ipath$s3_bucket))) logger::log_info("Overwriting S3 file ", path)
      logger::log_info("Uploading to S3 file ", path)
      aws.s3::put_object(object = path, file = path, bucket = ipath$s3_bucket, acl = "public-read", show_progress = TRUE)
      logger::log_info("Saved S3 file ", path)
    }
  )

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



