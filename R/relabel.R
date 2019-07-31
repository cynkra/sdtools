#' @export
relabel <- function(x, ...) {
  ll <- list(...)

  ll.spl <- strsplit(names(ll), split = ".", fixed = TRUE)
  dims.from <- vapply(ll.spl, function(e) e[1], "")
  labels.from <- vapply(ll.spl, function(e) if (length(e) == 1) NA_character_ else e[2], "")

  ll.spl <- strsplit(unlist(ll), split = ".", fixed = TRUE)
  dims.to <- vapply(ll.spl, function(e) e[1], "")
  labels.to <- vapply(ll.spl, function(e) if (length(e) == 1) NA_character_ else e[2], "")


  # renaming dims
  dfrom <- dims.from[is.na(labels.to)]
  dto <- dims.to[is.na(labels.to)]
  names(x$meta$labels$dimnames) <- relabel_vector(names(x$meta$labels$dimnames), dfrom, dto)
  names(x$meta$labels) <- relabel_vector(names(x$meta$labels), dfrom, dto)
  names(x$meta$hierarchy) <- relabel_vector(names(x$meta$hierarchy), dfrom, dto)
  names(x$meta$units) <- relabel_vector(names(x$meta$units), dfrom, dto)
  x$meta$dim.order <- relabel_vector(x$meta$dim.order, dfrom, dto)
  colnames(x$data) <- relabel_vector(colnames(x$data), dfrom, dto)

  # renaming labels
  ldim <- dims.from[!is.na(labels.to)]
  lfrom <- labels.from[!is.na(labels.to)]
  lto <- labels.to[!is.na(labels.to)]

  for (dim in unique(ldim)) {
    names(x$meta$labels[[dim]]) <-
      relabel_vector(names(x$meta$labels[[dim]]), lfrom[ldim == dim], lto[ldim == dim])
    swissdata::all_list_names(x$meta$hierarchy[[dim]]) <-
      relabel_vector(swissdata::all_list_names(x$meta$hierarchy[[dim]]), lfrom[ldim == dim], lto[ldim == dim])
    x$data[[dim]] <- relabel_vector(x$data[[dim]], lfrom[ldim == dim], lto[ldim == dim])
  }




  # renaming data


  x
}

# relabel_vector <- function(x, from, to) {
#   names(x) <- x
#   x[intersect(from, names(x))] <- to
#   unname(x)
# }


relabel_vector <- function(x, from, to) {
  if (is.null(x)) return(x)
  tibble(from = x) %>%
    left_join(tibble(from, to), by = "from") %>%
    mutate(ans = if_else(is.na(to), from, to)) %>%
    pull(ans)
}



