#' Write Swissdata Objects
#'
#' Functions for exporting swissdata objects by writing their contents to files.
#'
#' In this function the argument `path_out` specifies the top level directory.
#' The directory containing all the files for the saved swissdata object will be stored within `path_out` (see examples).
#'
#' @param x swissdata object to be written
#' @param path_out path for top-level directory for storing swissdata objects
#' @param test should `dataset_validate()` be run before writing the object (default = TRUE)
#' @param type format of meta data (`"json"` or `"yaml"`)
#'
#' @return If operation is successful the destination directory for the object is returned invisibly.
#'
#' @examples
#'  root_dir <- tempdir()
#'  data_dir <- dataset_write(adecco, root_dir)
#'  x <- dataset_read(data_dir)
#'  all.equal(x, adecco)
#'  # remove all created directories
#'  unlink(root_dir, recursive = TRUE)
#'
#' @author Christoph Sax
#' @name write
#' @export
dataset_write <- function(x, path_out, test = TRUE, type = c("json", "yaml")) {

  type <- match.arg(type)
  path_out_set <- file.path(path_out, x$set_id)
  ensure_path(path_out_set)

  if (test) ans <- dataset_validate(x)

  # save data
  data.table::fwrite(x$data, file.path(path_out_set, paste0(x$set_id, ".csv")))

  # save meta
  if (type == "yaml") {
    writeLines(yaml::as.yaml(x$meta), file.path(path_out_set, paste0(x$set_id, ".yaml")))
  } else {
    jsonlite::write_json(x$meta, file.path(path_out_set, paste0(x$set_id, ".json")), pretty = TRUE, auto_unbox = TRUE)
  }

  invisible(path_out_set)
}

