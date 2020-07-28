#' Create Swissdata Objects
#'
#' Creator funtion for swissdata objects. All  swissdata objects should be
#' created by this function.
#'
#' @param data data, or URL
#' @param meta meta, or URL
#' @param test should `dataset_validate()` be run on the newly-read object (default = TRUE)
#'
#' @return An object of class swissdata
#'
#' @examples
#'
#' dataset_create(adecco$data, adecco$meta, dataset_id = "ch_adecco_sjmi")
#' @export
dataset_create <- function(data, meta, dataset_id, test = TRUE) {

  # use seco style labeling and usage of .
  names(data) <- gsub(".", "_", names(data), fixed = TRUE)

  z <- list(
    meta = dots_to_underscore(empty_list_to_null(meta)),
    data = data,
    set_id = gsub(".", "_", dataset_id, fixed = TRUE)
  )
  names(z$meta) <- gsub("utc_updated", "updated_utc", names(z$meta), fixed = TRUE)

  class(z) <- "swissdata"

  if (test) ans <- dataset_validate(z)
  z

}
