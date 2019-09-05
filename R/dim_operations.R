#' Swissdata Dimension Operations
#'
#' A set of functions for manipulating dimensions of swissdata objects.
#'
#' `dim_add` adds an additional dimension to a swissdata object, `dim_drop`
#' removes a dimension, `dim_rename` renames an existing dimension.
#'
#' Internally these functions incorporate the requested changes into a given
#' swissdata object by altering the `x$data`, `x$meta$hierarchy`,
#' `x$meta$labels`, `x$meta$units`, and `x$meta$dim_order` fields.
#'
#' When removing the dimesnion using `dim_drop` only the rows corresponding to
#' provided level argument are retained in `x$data`.
#'
#' @param x swissdata object
#' @param dim dimension name
#' @param level single level ID from `dim`
#' @param label English label for the newly added dimension
#' @param name new dimension name
#'
#' @return a modified swissdata object
#'
#' @examples
#' # add new dimension "new_dim"
#' z <- adecco
#' z <- dim_add(z, dim = "new_dim", level = "nd", label = "newly added dim")
#' z$data
#' z$meta$dim_order
#' z$meta$labels$new_dim
#' z$meta$hierarchy$new_dim
#'
#' # rename the newly added dimension to "dim2"
#' z <- dim_rename(z, dim = "new_dim", name = "dim2")
#' z$data
#' z$meta$dim_order
#' z$meta$labels$dim2
#' z$meta$hierarchy$dim2
#'
#' # drop existing dimension idx_type (collapse to level "sch")
#' z <- dim_drop(z, dim = "idx_type", level = "sch")
#' z$data
#' z$meta$dim_order
#' z$meta$labels$idx_type
#' z$meta$hierarchy$idx_type
#'
#' @author Christoph Sax
#' @name dim_operations
#' @export
dim_add <- function(x, dim = "geo", level = "0", label = level) {
  stopifnot(all(names(x$data) != dim))

  x$data[[dim]] <- level
  x$meta$dim_order <- c(x$meta$dim_order, dim)
  x$data <- select(x$data, !! x$meta$dim_order, everything())
  x$meta$hierarchy[[dim]] <- stats::setNames(list(NULL), level)
  x$meta$labels[[dim]] <- stats::setNames(list(list(en = label)), level)
  x$meta$labels$dimnames <- c(x$meta$labels$dimnames, stats::setNames(list(list(en = dim)), dim))
  x
}


#' @rdname dim_operations
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


#' @rdname dim_operations
#' @export
dim_rename <- function(x, dim, name) {
  stopifnot(dim %in% setdiff(names(x$data), c("date", "value")))

  x$meta$dim_order <- replace(x$meta$dim_order, x$meta$dim_order==dim, name)
  names(x$data) <- replace(names(x$data), names(x$data)==dim, name)
  names(x$meta$hierarchy) <- replace(names(x$meta$hierarchy), names(x$meta$hierarchy)==dim, name)
  names(x$meta$labels) <- replace(names(x$meta$labels), names(x$meta$labels)==dim, name)
  names(x$meta$labels$dimnames) <- replace(names(x$meta$labels$dimnames), names(x$meta$labels$dimnames)==dim, name)
  names(x$meta$units) <- replace(names(x$meta$units), names(x$meta$units)==dim, name)

  x
}

