# set_path <- "/Users/christoph/git/swissdata/swissdata/wd/ch.fso.besta.mjr"
# read_swissdata(set_path)
#' @export
read_swissdata <- function(set_path) {

  files <- list.files(set_path, full.names = TRUE)
  file.yaml <- grep("yaml$", files, value = TRUE)
  file.csv <- grep("csv$", files, value = TRUE)

  meta <- yaml::yaml.load_file(file.yaml)
  data <- suppressMessages(readr::read_csv(file.csv))
  data <- readr::read_csv(file.csv, col_types = readr::cols(
    date = readr::col_date(format = ""),
    value = readr::col_double(),
    .default = readr::col_character()
  ))

  z <- list(meta = dots_to_underscore(empty_list_to_null(meta)), data = data)

  # use seco labeling
  names(z$meta) <- gsub("utc_updated", "updated_utc", names(z$meta), fixed = TRUE)

  class(z) <- "swissdata"

  ans <- test_swissdata(z)
  z
}

read_swissdata_json <- function(set_path) {

  files <- list.files(set_path, full.names = TRUE)
  file.json <- grep("json$", files, value = TRUE)
  file.csv <- grep("csv$", files, value = TRUE)

  meta <- jsonlite::read_json(file.json)
  data <- readr::read_csv(file.csv, col_types = readr::cols(
    date = readr::col_date(format = ""),
    value = readr::col_double(),
    .default = readr::col_character()
  ))

  z <- list(meta = empty_list_to_null(meta), data = data)

  class(z) <- "swissdata"
  z
}

#' @export
write_swissdata <- function(x, path) {
  ensure_path(path)
  swissdata::write_data_meta(z$data, z$meta, set_id = basename(path), .path_out = path)
}


empty_list_to_null <- function(x) {
  if (is.list(x) && length(x) == 0) return(NULL)
  if (!is.list(x)) return(x)
  lapply(x, empty_list_to_null)
}




dots_to_underscore <- function(x) {
  if (!is.list(x)) return(x)
  x <- setNames(x, gsub(".", "_", names(x), fixed = TRUE))
  lapply(x, dots_to_underscore)
}




