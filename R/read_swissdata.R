# set_path <- "/Users/christoph/git/swissdata/swissdata/wd/ch.fso.besta.mjr"
# read_swissdata(set_path)
#' @export
read_swissdata <- function(set_path, test = TRUE) {

  set_path <- normalizePath(set_path, mustWork = TRUE)

  files <- list.files(set_path, full.names = TRUE)
  file.yaml <- grep("yaml$", files, value = TRUE)
  stopifnot(length(file.yaml) == 1)
  file.csv <- grep("csv$", files, value = TRUE)
  stopifnot(length(file.csv) == 1)

  set_id <- gsub(".yaml", "", basename(file.yaml))
  stopifnot(identical(set_id, gsub(".csv", "", basename(file.csv))))

  meta <- yaml::yaml.load_file(file.yaml)
  data <- suppressMessages(readr::read_csv(file.csv))
  data <- readr::read_csv(file.csv, col_types = readr::cols(
    date = readr::col_date(format = ""),
    value = readr::col_double(),
    .default = readr::col_character()
  ))

  # use seco style labeling and usage of .
  names(data) <- gsub(".", "_", names(data), fixed = TRUE)

  z <- list(
    meta = dots_to_underscore(empty_list_to_null(meta)),
    data = data,
    set_id = gsub(".", "_", set_id, fixed = TRUE)
  )
  names(z$meta) <- gsub("utc_updated", "updated_utc", names(z$meta), fixed = TRUE)

  class(z) <- "swissdata"

  if (test) ans <- test_swissdata(z)
  z
}

#' @export
read_swissdata_json <- function(set_path) {

  set_path <- normalizePath(set_path, mustWork = TRUE)

  files <- list.files(set_path, full.names = TRUE)
  file.json <- grep("json$", files, value = TRUE)
  stopifnot(length(file.json) == 1)
  file.csv <- grep("csv$", files, value = TRUE)
  stopifnot(length(file.csv) == 1)

  set_id <- gsub(".json", "", basename(file.json))
  stopifnot(identical(set_id, gsub(".csv", "", basename(file.csv))))

  meta <- jsonlite::read_json(file.json)
  data <- readr::read_csv(file.csv, col_types = readr::cols(
    date = readr::col_date(format = ""),
    value = readr::col_double(),
    .default = readr::col_character()
  ))

  z <- list(
    meta = dots_to_underscore(empty_list_to_null(meta)),
    data = data,
    set_id = gsub(".", "_", set_id, fixed = TRUE)
  )

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




