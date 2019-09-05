#' Download and Read Swissdata Objects
#'
#' Functions for obtaining swissdata objects from different sources.
#' Possible input formats include: yaml files, json files,
#' and Amazon Simple Storage Service (s3)
#'
#' @param set_path directory containing the files to be read
#' @param test should `test_swissdata()` be run on the newly-read object (default = TRUE)
#' @param set_id swissdata series id used for reading the object from "Amazon s3".
#' @param bucket name for "Amazon s3" (default = "swissdata")
#'
#' @return An object of class swissdata
#'
#' @examples
#'   # read from yaml
#'   # set_path <- "some/path"
#'   # x <- read_swissdata_yaml(set_path)
#'
#'   # read from json
#'   # set_path <- "some/path"
#'   # x <- read_swissdata_json(set_path)
#'
#'   # read from s3
#'   x <- read_swissdata_s3("ch.fso.bapau")
#'
#' @author Christoph Sax
#' @name read
#' @export
read_swissdata_yaml <- function(set_path, test = TRUE) {

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

#' @rdname read
#' @export
read_swissdata_json <- function(set_path, test = TRUE) {

  set_path <- normalizePath(set_path, mustWork = TRUE)

  files <- list.files(set_path, full.names = TRUE)
  file.json <- grep("json$", files, value = TRUE)
  stopifnot(length(file.json) == 1)
  file.csv <- grep("csv$", files, value = TRUE)
  stopifnot(length(file.csv) == 1)

  set_id <- gsub(".json", "", basename(file.json))
  stopifnot(identical(set_id, gsub(".csv", "", basename(file.csv))))

  meta <- jsonlite::read_json(file.json)
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
  if (test) ans <- test_swissdata(z)
  message("successfully read: ", set_id)
  z
}

#' @rdname read
#' @export
read_swissdata_s3 <- function(set_id, bucket = "swissdata", test = TRUE) {

  base  <- paste0("https://sos-ch-dk-2.exo.io/", bucket, "/")
  files <- (paste0(base, set_id, "/", set_id, c(".csv", ".yaml")))

  tdir <- tempfile()
  dir.create(tdir)
  on.exit(unlink(tdir, recursive = TRUE))

  Map(download.file, url = files, destfile = file.path(tdir, basename(files)))

  ans <- read_swissdata_yaml(tdir, test = test)
  ans
}

