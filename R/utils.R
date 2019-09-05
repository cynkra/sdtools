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

# Transform Empty List Elements to Null
# x <- list(d = list(c = list(a = "aa", b = list())), f = list())
# empty_list_to_null(x)
empty_list_to_null <- function(x) {
  if (is.list(x) && length(x) == 0) return(NULL)
  if (!is.list(x)) return(x)
  lapply(x, empty_list_to_null)
}

# Transform Null List Elements to Empty String
# x <- list(d = list(c = list(a = "aa", b = NULL)), f = NULL)
# null_to_empty_string(x)
null_to_empty_string <- function(x) {
  if (is.null(x)) return("")
  if (!is.null(x) & !is.list(x)) return(x)
  lapply(x, null_to_empty_string)
}

# Change Dots in List Names to Underscore
# x <- list(d.1 = list(c.2 = list(a.3 = "aa", b.3 = NULL)), f.1 = NULL)
# dots_to_underscore(x)
dots_to_underscore <- function(x) {
  if (!is.list(x)) return(x)
  x <- setNames(x, gsub(".", "_", names(x), fixed = TRUE))
  lapply(x, dots_to_underscore)
}


