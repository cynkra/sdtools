#' @export
marry <- function(x, y) {
  newdata <-
    y$data %>%
    # only use those series from y that are not in x
    anti_join(x$data, by = setdiff(colnames(x$data), "value")) %>%
    bind_rows(x$data)

  z <- x

  z$meta$hierarchy <- merge_list2(y$meta$hierarchy, x$meta$hierarchy)
  z$meta$labels <- merge_list2(y$meta$labels, x$meta$labels)
  z$data <- newdata
  ans <- test_swissdata(z)
  z
}



# recursively merge list

# x <- list(d = list(c = list(a = "aa", b = "bb")))
# y <- list(d = list(c = list(a = "AA", c = "CC")))  # dominant
# merge_list2(x, y)
merge_list2 <- function(x, y) {
  if (!is.list(x)) {
    return(y)
  }
  # if list
  nn <- intersect(names(y), names(x))

  new.from.y <- y[!(names(y) %in% nn)]
  z <- x
  z[names(new.from.y)] <- new.from.y
  z[nn] <- Map(merge_list2, x = x[nn], y = y[nn])
  z
}

