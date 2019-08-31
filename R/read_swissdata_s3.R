#' Download and Read swissdata set
#' @export
#' @examples
#' # works without autentication!
#' x <- sdtools::read_swissdata_s3("ch.fso.bapau")
read_swissdata_s3 <- function(set_id, bucket = "swissdata") {

  base <- paste0("https://sos-ch-dk-2.exo.io/", bucket, "/")
  files <- (paste0(base, set_id, "/", set_id, c(".csv", ".yaml")))
  tdir <- tempfile()
  dir.create(tdir)

  Map(download.file, url = files, destfile = file.path(tdir, basename(files)))

  ans <- read_swissdata(tdir)
  # clean up
  unlink(tdir, recursive = TRUE)
  ans
}
