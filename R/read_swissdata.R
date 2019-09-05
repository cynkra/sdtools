# set_path <- "/Users/christoph/git/swissdata/swissdata/wd/ch.fso.besta.mjr"
# read_swissdata(set_path)
#' @export
#' @import dplyr
read_swissdata <- function(..., test = TRUE) {

  set_path <- file.path(...)

  set_path <- normalizePath(set_path, mustWork = TRUE)

  files <- list.files(set_path, full.names = TRUE)
  file.yaml <- grep("yaml$", files, value = TRUE)
  stopifnot(length(file.yaml) == 1)
  file.csv <- grep("csv$", files, value = TRUE)
  stopifnot(length(file.csv) == 1)

  set_id <- gsub(".yaml", "", basename(file.yaml))
  stopifnot(identical(set_id, gsub(".csv", "", basename(file.csv))))

  meta <- yaml::yaml.load_file(file.yaml)

  # SNB Series have NA details, but should have no 'details' entry
  if (!is.null(meta$details) && is.null(meta$details[[1]])) {
    meta$details <- NULL
  }
  if (!is.null(meta$details) && length(meta$details[[1]]) == 0) {
    meta$details <- NULL
  }

  if (!is.null(meta$units) && is.null(meta$units[[1]])) {
    meta$units <- list(all = list(en = " "))
  }
  if (!is.null(meta$units) && length(meta$units[[1]]) == 0) {
    meta$units <- list(all = list(en = " "))
  }
  if (is.null(meta$units)) {
    meta$units <- list(all = list(en = " "))
  }


  # data <- readr::read_csv(file.csv, col_types = readr::cols(
  #   date = readr::col_date(format = ""),
  #   value = readr::col_double(),
  #   .default = readr::col_character()
  # ))
  data <- as_tibble(data.table::fread(file.csv, colClasses=c(date = "Date", value = "numeric")))
  id_cols <- setdiff(names(data), c("date", "value"))
  data <-
    mutate(mutate_at(data, id_cols, as.character), date = as.Date(date))

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
  message("successfully read: ", set_id)
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
  # data <- readr::read_csv(file.csv, col_types = readr::cols(
  #   date = readr::col_date(format = ""),
  #   value = readr::col_double(),
  #   .default = readr::col_character()
  # ))
  data <- as_tibble(data.table::fread(file.csv, colClasses=c(date = "Date", value = "numeric")))
  id_cols <- setdiff(names(data), c("date", "value"))
  data <-
    mutate(mutate_at(data, id_cols, as.character), date = as.Date(date))


  z <- list(
    meta = dots_to_underscore(empty_list_to_null(meta)),
    data = data,
    set_id = gsub(".", "_", set_id, fixed = TRUE)
  )

  class(z) <- "swissdata"
  z
}

#' @export
write_swissdata <- function(x, path_out) {
  path_out_set <- file.path(path_out, x$set_id)
  ensure_path(path_out_set)
  test_swissdata(x)
  swissdata::write_data_meta(x$data, x$meta, set_id = x$set_id, .path_out = path_out_set)
}


