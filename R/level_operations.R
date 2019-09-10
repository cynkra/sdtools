#' Swissdata Dimension Level Operations
#'
#' A set of functions for manipulating dimension levels of swissdata objects.
#'
#' `level_drop` removes a level, `level_rename` renames an existing level.
#'
#' Internally these functions incorporate the requested changes into a given
#' swissdata object by altering the `x$data`, `x$meta$hierarchy`,
#' `x$meta$labels`, and `x$meta$units` fields.
#'
#' When removing the dimension level using `level_drop` the corresponding rows
#' from `x$data` are removed.
#'
#' @param x swissdata object
#' @param dim dimension name
#' @param level single level ID from `dim`
#' @param name new level name
#'
#' @return a modified swissdata object
#'
#' @examples
#'
#' # rename the existing level "ins" to "new" in dimension "idx_type"
#' z <- adecco
#' str(z)
#' z <- level_rename(z, dim = "idx_type", level = "ins", name = "new")
#' str(z)
#'
#' # drop existing level "new" from dimension "idx_type"
#' z <- level_drop(z, dim = "idx_type", level = "new")
#' str(z)
#'
#' @importFrom dplyr filter sym
#'
#' @author Karolis Koncevičius
#' @name level_operations
#' @export
level_drop <- function(x, dim, level) {
  stopifnot(dim %in% setdiff(names(x$data), c("date", "value")))
  stopifnot(level %in% x$data[[dim]])

  x$data <- filter(x$data, !! sym(dim) != level)

  hpos <- find_list_by_name(x$meta$hierarchy[[dim]], level)
  x$meta$hierarchy[[dim]][[hpos]] <- NULL
  x$meta$labels[[dim]][level] <- NULL

  x
}


#' @rdname level_operations
#' @export
level_rename <- function(x, dim, level, name) {
  stopifnot(dim %in% setdiff(names(x$data), c("date", "value")))
  stopifnot(level %in% x$data[[dim]])
  stopifnot(!(name %in% names(x$meta$labels[[dim]])))

  x$data[[dim]][x$data[[dim]]==level] <- name

  names(x$meta$labels[[dim]])[names(x$meta$labels[[dim]]) == level] <- name

  hierarchy <- x$meta$hierarchy[[dim]]
  hpos <- find_list_by_name(hierarchy, level)
  if(length(hpos) == 1) {
    names(hierarchy)[names(hierarchy) == level] <- name
  } else {
    hpos <- hpos[-length(hpos)]
    names(hierarchy[[hpos]])[names(hierarchy[[hpos]]) == level] <- name
  }
  x$meta$hierarchy[[dim]] <- hierarchy

  x
}
