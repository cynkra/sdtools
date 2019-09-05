#' @export
ensure_path <- function(path) {
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  file.remove(list.files(path, full.names = TRUE))
}

# Recursive Find in a List
# x <- list(d = list(c = list(a = "aa", b = "bb")), e = list(f = NA))
# rfind(x, "a")
# rfind(x, "f")
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

# Recursively Merge two Lists
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

