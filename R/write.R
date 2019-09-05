#' Write Swissdata Objects
#'
#' Functions for exporting swissdata objects by writing their contents to files.
#'
#' In this function the argument `path_out` specifies the top level directory.
#' The directory containing all the files for the saved swissdata object will be stored within `path_out` (see examples).
#'
#' @param x swissdata object to be written
#' @param path_out path for top-level directory for storing swissdata objects
#' @param test should `test_swissdata()` be run before writing the object (default = TRUE)
#'
#' @return If operation is successful the destination directory for the object is returned invisibly.
#'
#' @examples
#'  z <- adecco
#'  root_dir <- tempdir()
#'  data_dir <- write_swissdata(z, dir, test = FALSE)
#'  x <- read_swissdata_yaml(dir)
#'  str(x)
#'  str(z)
#'  # remove all created directories
#'  unlink(root_dir, recursive = TRUE)
#'
#' @author Christoph Sax
#' @name write
#' @export
write_swissdata <- function(x, path_out, test = TRUE) {
  path_out_set <- file.path(path_out, x$set_id)
  ensure_path(path_out_set)

  if (test) ans <- test_swissdata(x)

  # calculate hash of yaml to be generated (without the date as that MUST change)
  yaml_new <- yaml::as.yaml(x$meta)
  yaml_new_read <- readLines(textConnection(yaml_new))
  hash_new <- httr::sha1_hash("hush_swissdata", paste(yaml_new_read[1:(length(yaml_new_read) - 2)], collapse = "\n"))

  # if the hash of a previous version exists, compare
  yaml_path <- file.path(path_out_set, paste0(x$set_id, ".yaml"))
  if (file.exists(yaml_path)) {
    hash_old <- yaml::read_yaml(yaml_path)$sha1
    if(!is.null(hash_old) && hash_new != hash_old) {
      stop("Meta hash mismatch for: ", x$set_id, call. = FALSE)
    }
  }
  x$meta$sha1 <- hash_new

  # save data
  data.table::fwrite(x$data, file.path(path_out_set, paste0(x$set_id, ".csv")))

  # save meta
  writeLines(yaml::as.yaml(x$meta), file.path(path_out_set, paste0(x$set_id, ".yaml")))

  invisible(path_out_set)
}

