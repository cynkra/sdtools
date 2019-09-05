#' Compact Display of the Swissdata Object
#'
#' Compactly displays the internal hierarchy and labels of swissdata objects.
#'
#' @param x swissdata object
#' @param language language of the labels to be used for displaying the structure
#'
#' @return Does not return anything but displays the swissdata structure.
#'
#' @examples
#' z <- adecco
#' str(z)
#' str(z, languge = "de")
#'
#' @author Christoph Sax
#' @export
str.swissdata <- function(x, language = 'en') {

  meta <- x$meta
  labels <- x$meta$labels
  hierarchy <- meta$hierarchy

  # if a dim is missing in hierarchy, use names from labels
  dims.no.covered <- setdiff( names(labels), c(names(hierarchy), "dimnames"))
  hierarchy <- c(hierarchy, lapply(labels[dims.no.covered], function(x) lapply(x, function(x) list())))

  label.hierarchy <- hierarchy
  for(dm in setdiff(names(labels), "dimnames")) {
    pos <- rfind(label.hierarchy, dm)
    lab <- paste(dm, "-", labels$dimnames[[dm]][[language]])
    label.hierarchy[[pos]] <- list(lab, label.hierarchy[[pos]])
    for(lv in names(labels[[dm]])) {
      pos <- rfind(label.hierarchy, lv)
      pad <- (length(rfind(hierarchy, lv)) - 1) * 2
      lab <- paste(lv, "-", labels[[dm]][[lv]][[language]])
      lab <- paste0(strrep(" ", pad), lab)
      label.hierarchy[[pos]] <- list(lab, label.hierarchy[[pos]])
    }
  }

  cat(paste0(unlist(label.hierarchy), "\n", collapse=""))
}


