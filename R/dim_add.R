#' Add New Dimension
#'
#' Adds an additional dimension to the provided Swissdata object.
#'
#' Given an existing Swissdata object and additional parameters of
#' dimension name, dimension level, and dimension label this function
#' incorporates the newly specified dimension by altering the \code{x$data},
#' \code{x$meta$hierarchy}, \code{x$meta$labels}, and \code{x$meta$dim_order}
#' fields.
#'
#' @param x swissdata object
#' @param dim name of dimension to be added (default = "geo")
#' @param level level id used for the newly added dimension (default = "0")
#' @param label English label for the newly added dimension (default = \code{level})
#'
#' @return a modified swissdata object that includes newly added dimension
#'
#' @seealso \code{dim_drop}
#'
#' @author Christoph Sax
#' @export
dim_add <- function(x, dim = "geo", level = "0", label = level) {
  stopifnot(all(names(x$data) != dim))

  x$data[[dim]] <- level
  x$meta$dim_order <- c(x$meta$dim_order, dim)
  x$data <- select(x$data, !! x$meta$dim_order, everything())
  x$meta$hierarchy[[dim]] <- level
  x$meta$labels[[dim]] <- setNames(list(list(en = label)), level)
  x$meta$labels$dimnames <- c(x$meta$labels$dimnames, setNames(list(list(en = dim)), dim))
  x
}


#' Drop Existing Dimension
#'
#' Drops an existing dimension from the provided Swissdata object.
#'
#' Given an existing Swissdata object and a dimension name, this function
#' removes this dimension from the data by modifying the \code{x$data},
#' \code{x$meta$hierarchy}, \code{x$meta$labels}, and \code{x$meta$dim_order}
#' fields. When removing the dimesnion from \code{x$data} only the rows
#' corresponding to provided level argument are retained
#'
#' @param x swissdata object
#' @param dim name of dimension to be removed (default = "trans")
#' @param level level id from the dropped dimension that will be retained (default = "ind")
#'
#' @return a modified swissdata object with the specified dimension removed.
#'
#' @seealso \code{dim_add}
#'
#' @author Christoph Sax
#' @export
dim_drop <- function(x, dim = "trans", level = "ind") {
  stopifnot(level %in% x$data[[dim]])

  x$data <-
    x$data %>%
    filter(!! sym(dim) == level) %>%
    select(- !! sym(dim))

  x$meta$dim_order <- setdiff(x$meta$dim_order, dim)
  x$meta$hierarchy[dim] <- NULL
  x$meta$labels[dim] <- NULL
  x$meta$labels$dimnames[dim]<- NULL

  if (dim %in% names(x$meta$units)) {
    x$meta$units$all <- x$meta$units[[dim]][[level]]
    x$meta$units[dim] <- NULL
  }
  x
}

