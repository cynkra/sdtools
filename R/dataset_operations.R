#' Swissdata Dataset Operations
#'
#' A set of functions for manipulating swissdata datasets.
#'
#' `dataset_merge` combines the dimension levels from two swissdata datasets.
#'
#' `dataset_merge` adds dimension levels that are present only in `y` to `x`.
#' As a result in the case where the same dimensions or levels are present
#' in both of the objects - the one from `x` is selected. `dataset_validate()`
#' function is executed before returning the output in order to make sure the
#' combination of the two objects was successful.
#'
#' @param x original swissdata object
#' @param y additional swissdata object
#'
#' @return a combined swissdata object
#'
#' @examples
#' # merge two distinct datasets
#' z1 <- adecco
#' str(z1)
#' z2 <- level_rename(z1, "idx_type", "ins", "new")
#' str(z2)
#' z <- dataset_merge(z1, z2)
#' str(z)
#'
#' @importFrom dplyr anti_join bind_rows
#' @importFrom magrittr %>%
#'
#' @author Christoph Sax
#' @export
dataset_merge <- function(x, y) {
  newdata <-
    y$data %>%
    # only use those series from y that are not in x
    anti_join(x$data, by = setdiff(colnames(x$data), "value")) %>%
    bind_rows(x$data)

  z <- x

  z$data <- newdata
  z$meta$hierarchy <- merge_two_lists(y$meta$hierarchy, x$meta$hierarchy)
  z$meta$labels <- merge_two_lists(y$meta$labels, x$meta$labels)
  dataset_validate(z)
  z
}

