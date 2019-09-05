#' Marry Two Swissdata Objects
#'
#' Combine the dimension levels from two swissdata objects.
#'
#' The function adds dimension levels that are present only in `y` to `x`.
#' As a result in the case where the same dimensions or levels are present
#' in both of the objects - the one from `x` is selected.
#'
#' `test_swissdata()` function is executed before returning the output
#' in order to make sure the combination of the two objects was successful.
#'
#' @param x original swissdata object
#' @param y additional swissdata object
#'
#' @return a combined swissdata object
#'
#' @examples
#' z  <- adecco
#' z2 <- level_rename(z, "idx_type", "ins", "new")
#' marry(z, z2)
#'
#' @importFrom dplyr anti_join bind_rows
#' @importFrom magrittr %>%
#'
#' @author Christoph Sax
#' @export
marry <- function(x, y) {
  newdata <-
    y$data %>%
    # only use those series from y that are not in x
    anti_join(x$data, by = setdiff(colnames(x$data), "value")) %>%
    bind_rows(x$data)

  z <- x

  z$data <- newdata
  z$meta$hierarchy <- merge_two_lists(y$meta$hierarchy, x$meta$hierarchy)
  z$meta$labels <- merge_two_lists(y$meta$labels, x$meta$labels)
  test_swissdata(z)
  z
}

