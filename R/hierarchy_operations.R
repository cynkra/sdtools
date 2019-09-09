#' Swissdata Hierarchy Operations
#'
#' A set of functions for manipulating hierarchy levels of swissdata objects.
#'
#' `hierarchy_add` adds a new level to the hierarchy, `hierarchy_move` moves
#' an existing level to a new parent, `hierarchy_reorder` changes the order of
#' under specified parent level.
#'
#' Internally these functions incorporate the requested changes into a given
#' swissdata object by altering the `x$hierarchy` field.
#'
#' In all cases when parent is `NULL` or missing the top level of the hierarchy
#' is assumed. For `hierarchy_reorder` when `order` is only specified for a few
#' available children - the provided levels are reordered and the rest are moved
#' to the back of the hierarchy.
#'
#' @param x swissdata object
#' @param dim dimension name
#' @param level single level ID from `dim`
#' @param order a character vector specifying the order of levels in the hierarchy
#' @param label English label for the newly added hierarchy level
#' @param parent single level ID from `dim` where the `level` will be added.
#' If left empty then the top position in the hierarchy is assumed.
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
#' # reorder the children within "sch"
#' z <- hierarchy_reorder(z, dim = "idx_type", order = c("ins", "unw", "pzua"), parent = "sch")
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
    stop("level '", level, "' already exists in the hierarchy")
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

  child_pos <- find_list_by_name(hierarchy, level)
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


#' @rdname hierarchy_operations
#' @export
hierarchy_reorder <- function(x, dim, order, parent) {
  stopifnot(all(!duplicated(order)))
  hierarchy <- x$meta$hierarchy[[dim]]

  if(missing(parent) || is.null(parent)) {
    parent_pos <- NULL
  } else {
    parent_pos <- find_list_by_name(hierarchy, parent)
    if(is.null(parent_pos)) {
      stop("the specified parent level does not exist in the hierarchy")
    }
  }

  if(is.null(parent_pos)) {
    stopifnot(all(order %in% names(hierarchy)))
    order <- c(order, setdiff(names(hierarchy), order))
    hierarchy <- hierarchy[order]
  } else {
    stopifnot(all(order %in% names(hierarchy[[parent_pos]])))
    order <- c(order, setdiff(names(hierarchy[[parent_pos]]), order))
    hierarchy[[parent_pos]] <- hierarchy[[parent_pos]][order]
  }

  x$meta$hierarchy[[dim]] <- hierarchy
  x
}
