#' @export
write_swissdata <- function(x, path_out) {
  path_out_set <- file.path(path_out, x$set_id)
  ensure_path(path_out_set)
  test_swissdata(x)
  swissdata::write_data_meta(x$data, x$meta, set_id = x$set_id, .path_out = path_out_set)
}


