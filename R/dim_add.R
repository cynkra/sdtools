#' Add or Drop Dimensions
#'
#' `dim_add` adds an additional dimension to a Swissdata object, `dim_drop`
#' removes a dimension.
#'
#' Given an existing Swissdata object and additional parameters of
#' dimension name, dimension level, and dimension label this function
#' incorporates the newly specified dimension by altering the `x$data`,
#' `x$meta$hierarchy`, `x$meta$labels`, and \code{x$meta$dim_order`
#' fields. When removing the dimesnion from `x$data` only the rows
#' corresponding to provided level argument are retained
#'
#' @param x swissdata object
#' @param dim name of dimension to be added or removed
#' @param level single level ID to be used in the new dimension (`dim_add`), or
#'   single level ID for which the data is filtered (`dim_drop`).
#' @param label English label for the newly added dimension
#'
#' @return a modified swissdata object
#'
#' @example
#'   # add new dimension "new_dim"
#'   z <- adecco
#'   z <- dim_add(z, dim = "new_dim", level = "nd", label = "newly added dim")
#'   z$data
#'   z$meta$dim_order
#'   z$meta$labels$new_dim
#'   z$meta$hierarchy$new_dim
#'
#'   # drop existing dimension idx_type (collapse to level "sch")
#'   z <- dim_drop(z, dim = "idx_type", level = "sch")
#'   z$data
#'   z$meta$dim_order
#'   z$meta$labels$idx_type
#'   z$meta$hierarchy$idx_type
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


#' @name dim_add
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

