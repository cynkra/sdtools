#' Swissdata Hierarchy Operations
#'
#' A set of functions for manipulating hierarchy levels of swissdata objects.
#'
#' `hierarchy_add` adds a new grouping level to the hierarchy, `hierarchy_move`
#' changes the hierarchy positoin of the specific dimension level.
#'
#' Internally these functions incorporate the requested changes into a given
#' swissdata object by altering the `x$hierarchy` field.
#'
#' @param x swissdata object
#' @param dim dimension name
#' @param level single level ID from `dim`
#' @param label English label for the newly added hierarchy level
#' @param parent single leved ID from `dim` where the `level` will be added.
#' If left empty then the top position in the hierarchy is assumed.k
#'
#' @return a modified swissdata object
#'
#' @examples
#' # add new label "d0" to top level of "idx_type"
#' z <- adecco
#' str(z)
#' z <- hierarchy_add(z, dim = "idx_type", level = "d0", label = "new group")
#' str(z)
#' # make the newly added hierarchy level the child of "unw"
#' z <- hierarchy_move(z, dim = "idx_type", level = "d0", parent = "unw")
#' str(z)
#' # move the whole "sch" sub-hierarchy within "scs"
#' z <- hierarchy_move(z, dim = "idx_type", level = "sch", parent = "scs")
#' str(z)
#'
#' @author Karolis Koncevičius
#' @name hierarchy_operations
#' @export
hierarchy_add <- function(x, dim, level, parent, label = level) {
  hierarchy <- x$meta$hierarchy[[dim]]

  if(missing(parent) || is.null(parent)) {
    parent_pos <- NULL
  } else {
    parent_pos <- find_list_by_name(hierarchy, parent)
    if(is.null(parent_pos)) {
      stop("the specified parent level does not exist in the hierarchy")
    }
    if(level == parent) {
      stop("element cannot be a parent of itself")
    }
  }

  child_pos <- find_list_by_name(hierarchy, level)
  if(!is.null(child_pos)) {
    stop("level '", level, "' already axists in the hierarchy")
  }
  child <- stats::setNames(list(NULL), level)

  if(is.null(parent_pos)) {
    hierarchy <- append(hierarchy, child)
  } else {
    hierarchy[[parent_pos]] <- append(hierarchy[[parent_pos]], child)
  }

  x$meta$hierarchy[[dim]] <- hierarchy
  x$meta$labels[[dim]][[level]] <- list(en = label)
  x
}


#' @rdname hierarchy_operations
#' @export
hierarchy_move <- function(x, dim, level, parent) {
  hierarchy <- x$meta$hierarchy[[dim]]

  if(missing(parent) || is.null(parent)) {
    parent_pos <- NULL
  } else {
    parent_pos <- find_list_by_name(hierarchy, parent)
    if(is.null(parent_pos)) {
      stop("the specified parent level does not exist in the hierarchy")
    }
    if(level == parent) {
      stop("element cannot be a parent of itself")
    }
  }

  child_pos  <- find_list_by_name(hierarchy, level)
  child <- stats::setNames(list(hierarchy[[child_pos]]), level)

  if(is.null(parent_pos)) {
    hierarchy <- append(hierarchy, child)
  } else {
    hierarchy[[parent_pos]] <- append(hierarchy[[parent_pos]], child)
  }
  hierarchy[[child_pos]] <- NULL

  x$meta$hierarchy[[dim]] <- hierarchy
  x
}
