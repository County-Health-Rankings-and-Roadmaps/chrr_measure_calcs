


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
  if (download_status != 0) {
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

#' Numerical comparison
almost_equal <- function(x, y, rel_tol = 0.001, abs_tol = Inf) {
  abs_dif <- abs(x - y)
  rel_dif <- ifelse(x == 0, 0, abs_dif / abs((x + y) / 2))
  (is.na(x) & is.na(y)) | !is.na(x) & !is.na(y) & abs_dif <= abs_tol & rel_dif <= rel_tol
}
