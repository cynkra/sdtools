#' Download and Read Swissdata Objects
#'
#' Functions for obtaining swissdata objects from different sources.
#' Possible input formats include: yaml files, json files,
#' and Amazon Simple Storage Service (s3)
#'
#' @param set_path directory containing the files to be read
#' @param test should `dataset_validate()` be run on the newly-read object (default = TRUE)
#' @param set_id swissdata series id used for reading the object from "Amazon s3".
#' @param bucket name for "Amazon s3" (default = "swissdata")
#'
#' @return An object of class swissdata
#'
#' @examples
#'
#' tf <- tempfile()
#' dataset_write(adecco, tf)
#' dataset_read(file.path(tf, "ch_adecco_sjmi"))
#'
#' # read from s3
#' dataset_read_s3("ch_adecco_sjmi", bucket = "dataseries")
#' dataset_read_s3("ch_adecco_sjmi", bucket = "swissdata")
#'
#' @importFrom dplyr mutate mutate_at as_tibble
#' @importFrom readr read_csv cols col_date col_double col_character
#'
#' @author Christoph Sax
#' @name read
#' @export
dataset_read <- function(set_path, test = TRUE) {

  set_path <- normalizePath(set_path, mustWork = TRUE)

  files <- list.files(set_path, full.names = TRUE)

  file.yaml.or.json <- grep("(\\.yaml$)|(\\.json$)", files, value = TRUE)
  stopifnot(length(file.yaml.or.json) == 1)
  file.csv <- grep("csv$", files, value = TRUE)
  stopifnot(length(file.csv) == 1)

  set_id <- gsub("(\\.yaml$)|(\\.json$)", "", basename(file.yaml.or.json))
  stopifnot(identical(set_id, gsub(".csv", "", basename(file.csv))))

  if (grepl("\\.json$", file.yaml.or.json)) {
    meta <- jsonlite::read_json(file.yaml.or.json)
  } else {
    meta <- yaml::yaml.load_file(file.yaml.or.json)
  }

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
  data <-   readr::read_csv(file.csv, col_types = cols(
    date = readr::col_date(format = ""),
    value = readr::col_double(),
    .default = readr::col_character()
  ))
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

  if (test) ans <- dataset_validate(z)
  message("successfully read: ", set_id)
  z
}


#' @rdname read
#' @export
dataset_read_yaml <- function(set_path, test = TRUE) {
  .Deprecated("dataset_read")
  dataset_read(set_path, test = test)
}


#' @rdname read
#' @export
dataset_read_s3 <- function(set_id, bucket = "swissdata", test = TRUE) {

  base  <- paste0("https://sos-ch-dk-2.exo.io/", bucket, "/")
  files <- (paste0(base, set_id, "/", set_id, c(".csv", ".yaml")))

  tdir <- tempfile()
  dir.create(tdir)
  on.exit(unlink(tdir, recursive = TRUE))

  Map(utils::download.file, url = files, destfile = file.path(tdir, basename(files)))

  ans <- dataset_read(tdir, test = test)
  ans
}

