#' @export
ensure_path <- function(path) {
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  file.remove(list.files(path, full.names = TRUE))
}

# Recursive Find in a List
rfind <- function(x, name, pos=c()) {
  ind <- match(name, names(x))
  if (!is.na(ind)) return(c(pos, ind))
  for (i in seq_along(x)) {
    if (class(x[i]) == "list") {
      out <- Recall(x[[i]], name, pos)
      if (!is.null(out)) return(c(i, out))
    }
  }
}
