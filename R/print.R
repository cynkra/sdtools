#' Print Swissdata Object
#'
#' Function for printing swissdata objects
#'
#' @param x swissdata object
#' @param language language to be used for printing the labels
#'
#' @return invisibly returns the swissdata object
#'
#' @examples
#' z <- adecco
#' print(z)
#' print(z, languge = "de")
#'
#' @seealso `str.swissdata`
#'
#' @author Christoph Sax
#' @export
print.swissdata <- function(x, language = 'en') {

  print(head(x$data))
  cat("\n")
  str(x, language)

  invisible(x)
}

