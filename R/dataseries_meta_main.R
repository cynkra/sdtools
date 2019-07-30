#' @export
dataseries_meta_main <- function(x) {

  set_id <- x$meta$set_id
  message(set_id)

  meta <- x$meta
  data <- x$data

  # fr.number <- unique(tsbox::ts_summary(data)$freq)
  # curde way for freq determination. should be solved in tsbox
  fr_number <- frequency(tsbox::ts_ts(data.table(time = unique(data$date), value = 0)))

  fr_map <- c(
    `1` = "yearly",
    `4` = "quarterly",
    `12` = "monthly",
    `365.2425` = "daily"
  )

  fr_name <- unname(fr_map[as.character(fr_number)])


  null_to_empty <- function(x) {
    if (is.null(x)){
      x <- ""
    }
    x
  }

  english_if_possible <- function(e) {
    if (is.character(e)) return(e)
    z <- e$en
    if (is.null(z)) z <- e$de
    z
  }

  data.table(
    var = gsub("\\.", "_", set_id),
    label = english_if_possible(meta$title),
    searchlabel = english_if_possible(meta$title),
    sublabel = "",
    category = meta$dataseries$category,
    source = meta$source_name$en,
    link = paste0(
      "<a href = '",
      meta$source_url,
      "' target = '_blank'>",
      meta$source_name$en,
      "</a>"
    ),
    descr = null_to_empty(meta$details$en),
    freq = fr_name,
    show = 1
  )

}




