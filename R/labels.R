#' Flat labels
#'
#' @export
#' @examples
#' flat_labels(adecco$meta$labels)
flat_labels <- function(labels) {
  bind_rows(lapply(labels, flat_label), .id = "dim")
}

flat_label <- function(label) {
  list_to_tbl <- function(e) {
    as_tibble(data.frame(null_to_empty_string(e), stringsAsFactors = FALSE))
  }
  bind_rows(lapply(label, list_to_tbl), .id = "id")
}

