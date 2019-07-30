#' @export
dim_add <- function(x, dim = "geo", level = "0") {
  x$data[[dim]] <- level
  x$meta$dim.order <- c(x$meta$dim.order, dim)
  x$data <- select(x$data, !! x$meta$dim.order, everything())
  x$meta$labels[[dim]] <- setNames(list(list(en = level)), level)
  x$meta$labels$dimnames <- c(x$meta$labels$dimnames, setNames(list(list(en = dim)), dim))
  x
}
