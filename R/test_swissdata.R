# basics from swissdata, extended

#' @export
test_swissdata <- function(x) {

  stopifnot("set_id" %in% names(x))
  if (grepl(".", x$set_id, fixed = TRUE)) stop("set_id must not contain dots (.)")

  data <- x$data
  meta <- x$meta

  stopifnot(inherits(data, "tbl_df"))
  stopifnot(inherits(meta, "list"))

  # Has meta the correct entries?
  have.tos <- c("title", "source_name", "source_url", "units", "dim_order", "aggregate", "labels", "updated_utc")
  nice.to.haves <- c("details", "hierarchy", "sha1", "dataseries")
  if (length(setdiff(have.tos, names(meta))) > 0){
    stop(
      "missing elements in meta: ",
      paste(setdiff(have.tos, names(meta)), collapse = ", "),
      call. = FALSE
    )
  }
  if (length(setdiff(names(meta), c(have.tos, nice.to.haves))) > 0){
    stop(
      "invalid elements in meta: ",
      paste(setdiff(names(meta), c(have.tos, nice.to.haves)), collapse = ", "),
      call. = FALSE
    )
  }

  # Has data the correct final columns
  stopifnot(colnames(data)[ncol(data)] == "value")
  stopifnot(colnames(data)[ncol(data)-1] == "date")

  # Are the dimensions in data and meta identical?
  dims <- colnames(data)[1:(ncol(data)-2)]
  dims.meta <- setdiff(names(meta$labels), "dimnames")
  stopifnot(identical(sort(dims), sort(dims.meta)))


  for (dim.i in dims){

    # Are labels in meta unique?
    if (anyDuplicated(names(meta$labels[[dim.i]])) > 0) {
      stop("non unique labels in :", dim.i)
    }

    # Do data ids have a label in meta
    if (length(setdiff(unique(data[[dim.i]]), names(meta$labels[[dim.i]]))) > 0){
      stop(
        "data meta mismatch - values in dim '", dim.i, "' differ: \n\ndata: ",
        paste(unique(data[[dim.i]]), collapse = ", "), "\n\nmeta: ",
        paste(names(meta$labels[[dim.i]]), collapse = ", "),
        call. = FALSE
      )
    }
  }

  # are the key columns unique?
  is_duplicated <- duplicated(select(data, -value))
  if (sum(is_duplicated) > 0) {
    duplicates <- distinct(select(data, -value, -date)[is_duplicated, ])
    stop(
      "Duplicates in series: \n\n",
      paste(capture.output(duplicates), collapse = "\n"),
      call. = FALSE
    )
  }

  # Do we have labels for dimnames?
  stopifnot("dimnames" %in% names(meta$labels))

  if (!identical(sort(names(meta$labels$dimnames)), sort(dims))) {
    stop(
      "dim mismatch \ndata: ",
      paste(sort(dims), collapse = ", "), "\nmeta dimnames : ",
      paste(sort(names(meta$labels$dimnames)), collapse = ", ")
    )
  }

  # Are languages speficied correctyl
  .supported.lang <- c("en", "de", "fr", "it")   # better store centrally

  lang_spec_ok <- function(x) {
    identical(names(x), intersect(.supported.lang, names(x)))
  }
  all_lang_spec_ok <- function(x) {
    vapply(x, lang_spec_ok, TRUE)
  }
  lang.ok <- unlist(lapply(meta$labels, all_lang_spec_ok))
  long.not.ok <- names(lang.ok)[!lang.ok]
  if (length(long.not.ok) > 0){
    stop("misspecified languages: ", paste(long.not.ok, collapse = ", "), call. = FALSE)
  }
  stopifnot(lang_spec_ok(meta$title))
  stopifnot(lang_spec_ok(meta$source.name))

  # Specify URL correctly
  stopifnot(grepl("http", meta$source.url))

  if (!is.null(meta$details)){
    stopifnot(lang_spec_ok(meta$details))
  }

  # TODO more test on hierarchy



  # if hierarchy is specified, is it complete?
  # are all hierarchy elememnts in data?
  # are all data ids in hierarchy?

  # TODO more test on units

  x

}
