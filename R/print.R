#' Print Swissdata Object
#'
#' Function for printing swissdata objects
#'
#' @param x swissdata object
#' @param language language to be used for printing the labels
#' @param ... further arguments passed to or from methods
#'
#' @return invisibly returns the swissdata object
#'
#' @examples
#' z <- adecco
#' print(z)
#' print(z, language = "de")
#'
#' @seealso `str.swissdata`
#'
#' @author Karolis Koncevičius
#' @export
print.swissdata <- function(x, language = 'en', ...) {

  print(utils::head(x$data))
  cat("\n")
  str.swissdata(x, language)

  invisible(x)
}


#' Compact Display of the Swissdata Object
#'
#' Compactly displays the internal hierarchy and labels of swissdata objects.
#'
#' @param object swissdata object
#' @param language language of the labels to be used for displaying the structure
#' @param ... further arguments passed to or from methods
#'
#' @return Does not return anything but displays the swissdata structure.
#'
#' @examples
#' z <- adecco
#' str(z)
#' str(z, language = "de")
#'
#' @author Karolis Koncevičius
#' @export
str.swissdata <- function(object, language = 'en', ...) {

  meta <- object$meta
  labels <- object$meta$labels
  hierarchy <- meta$hierarchy

  # if a dim is missing in hierarchy, use names from labels
  dims.no.covered <- setdiff( names(labels), c(names(hierarchy), "dimnames"))
  hierarchy <- c(hierarchy, lapply(labels[dims.no.covered], function(x) lapply(x, function(x) list())))

  label.hierarchy <- hierarchy
  for(dm in setdiff(names(labels), "dimnames")) {
    pos <- find_list_by_name(label.hierarchy, dm)
    lab <- paste(dm, "-", labels$dimnames[[dm]][[language]])
    label.hierarchy[[pos]] <- list(lab, label.hierarchy[[pos]])
    for(lv in names(labels[[dm]])) {
      pos <- find_list_by_name(label.hierarchy[[dm]], lv)
      pad <- length(find_list_by_name(hierarchy[[dm]], lv)) * 2
      lab <- paste(lv, "-", labels[[dm]][[lv]][[language]])
      lab <- paste0(strrep(" ", pad), lab)
      label.hierarchy[[dm]][[pos]] <- list(lab, label.hierarchy[[dm]][[pos]])
    }
  }

  cat(paste0(unlist(label.hierarchy), "\n", collapse=""))
}

