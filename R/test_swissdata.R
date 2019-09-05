#' Test Swissdata
#'
#' Extended set of tests for swissdata objects.
#'
#' The function runs several tests on a swissdata object:\cr\cr
#' 1. "data" slot is a `tibble`\cr
#' 2. "meta" slot is a `list`\cr
#' 3. "meta" slot has all the required entries\cr
#' 4. "meta" slot has no unexpected entries\cr
#' 5. there are no missing values in `x$data$value`\cr
#' 6. there are no missing values in `x$data$date`\cr
#' 7. dates are specified as the starts of period\cr
#' 8. last elements of "data" slot are "date" and "value"\cr
#' 9. "meta" and "data" has identical dimensions\cr
#' 10. all dimensions have unique labels in "meta"\cr
#' 11. all dimensions in data have labels in "meta"\cr
#' 12. there are no duplicated entries for the same date\cr
#' 13. each dimension has it's own label\cr
#' 14. languages are specified correctly\cr
#' 15. source URL has a "http" in it\cr
#'
#' @param x swissdata object
#'
#' @return If all tests passed the original swissdata object is returned invisibly.
#' Otherwise the execution is halted and an informative error message is displayed instead.
#'
#' @examples
#' z <- adecco
#' test_swissdata(z)
#'
#' @author Christoph Sax
#' @export
test_swissdata <- function(x) {
  stopifnot("set_id" %in% names(x))
  if (grepl(".", x$set_id, fixed = TRUE)) stop("set_id must not contain dots (.)")

  data <- x$data
  meta <- x$meta

  # data must be a tibble
  stopifnot(inherits(data, "tbl_df"))

  # meta must be a list
  stopifnot(inherits(meta, "list"))

  # meta must have all the required fields
  have.tos <- c("title", "source_name", "source_url", "units", "dim_order", "aggregate", "labels", "updated_utc")
  nice.to.haves <- c("details", "hierarchy", "sha1", "dataseries")
  if (length(setdiff(have.tos, names(meta))) > 0) {
    stop(
      "missing elements in meta: ",
      paste(setdiff(have.tos, names(meta)), collapse = ", "),
      call. = FALSE
      )
  }

  # meta cannot have unexpected fields
  if (length(setdiff(names(meta), c(have.tos, nice.to.haves))) > 0) {
    stop(
      "invalid elements in meta: ",
      paste(setdiff(names(meta), c(have.tos, nice.to.haves)), collapse = ", "),
      call. = FALSE
      )
  }

  # values cannot have NAs
  stopifnot(!anyNA(data$value))

  # dates cannot have NAs
  stopifnot(!anyNA(data$date))

  # are the dates start of period?
  days <- unique(as.POSIXlt(data$date)$mday)
  if (length(days) < 6 && !(1 %in% days)) {
    stop("dates need to be first of period")
  }

  # has data the correct final columns
  stopifnot(colnames(data)[ncol(data)] == "value")
  stopifnot(colnames(data)[ncol(data)-1] == "date")

  # are the dimensions in data and meta identical?
  dims <- colnames(data)[1:(ncol(data)-2)]
  dims.meta <- setdiff(names(meta$labels), "dimnames")
  if (!(identical(sort(dims), sort(dims.meta)))) {
    stop(
      "dimensions in data (",
      paste(sort(dims), collapse = ", "),
      ") differ from those in meta$labels (",
      paste(sort(dims.meta), collapse = ", "),
      ")",
      call. = FALSE
      )
  }


  for (dim.i in dims) {
    # are labels in meta unique?
    if (anyDuplicated(names(meta$labels[[dim.i]])) > 0) {
      stop("non unique labels in :", dim.i)
    }

    # do data ids have a label in meta
    if (length(setdiff(unique(data[[dim.i]]), names(meta$labels[[dim.i]]))) > 0) {
      stop(
        "data meta mismatch - values in dim '", dim.i, "' differ: \n\ndata: ",
        paste(unique(data[[dim.i]]), collapse = ", "), "\n\nmeta$labels: ",
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

  # do we have labels for dimnames?
  stopifnot("dimnames" %in% names(meta$labels))
  if (!identical(sort(names(meta$labels$dimnames)), sort(dims))) {
    stop(
      "dim mismatch \ndata: ",
      paste(sort(dims), collapse = ", "), "\nmeta dimnames : ",
      paste(sort(names(meta$labels$dimnames)), collapse = ", ")
      )
  }

  # are languages specified correctly
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

  if (!is.null(meta$details)){
    stopifnot(lang_spec_ok(meta$details))
  }

  # specify URL correctly
  stopifnot(grepl("http", meta$source.url))

  # TODO more test on hierarchy

  # if hierarchy is specified, is it complete?
  # are all hierarchy elememnts in data?
  # are all data ids in hierarchy?

  # TODO more test on units

  invisible(x)
}
