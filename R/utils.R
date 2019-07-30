#' @export
ensure_path <- function(path) {
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  file.remove(list.files(path, full.names = TRUE))
}

#' @export
test_swissdata <- function(x) {
  test_data_meta2(x$data, x$meta)
}
