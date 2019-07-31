#' @export
dim_add <- function(x, dim = "geo", level = "0", label = level) {
  x$data[[dim]] <- level
  x$meta$dim_order <- c(x$meta$dim_order, dim)
  x$data <- select(x$data, !! x$meta$dim_order, everything())
  x$meta$labels[[dim]] <- setNames(list(list(en = level)), label)
  x$meta$labels$dimnames <- c(x$meta$labels$dimnames, setNames(list(list(en = dim)), dim))
  x
}
