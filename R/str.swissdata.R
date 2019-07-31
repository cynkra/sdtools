
#' @export
#' @method str swissdata
str.swissdata <- function(x) {

  language <- 'en'

  meta <- x$meta
  hierarchy <- meta$hierarchy
  # if a dim is missing in hierarchy, use names from labels
  dims.no.covered <- setdiff( names(x$meta$labels), c(names(hierarchy), "dimnames"))

  hierarchy <- c(hierarchy, lapply(x$meta$labels[dims.no.covered], function(x) lapply(x, function(x) list())))

  dim_id_label <- list_to_dim_id_label(meta$label)
  dim_id_label_switched_raw <-
    dim_id_label %>%
    rename(value = !!language) %>%
    rename(en = id) %>%
    rename(id = value) %>%
    select(dim, id, en)
  dim_id_label_switched <- bind_rows(filter(dim_id_label, dim ==
      "dimnames"), filter(dim_id_label_switched_raw, dim !=
      "dimnames"))
  ans <- translate_hierarchy_with_id(hierarchy, dim_id_label_switched)
  cat(gsub(": ~", "", gsub(": {}", "", yaml::as.yaml(ans), fixed = TRUE)))

}


# copied from swissdata::translate_hierarchy and adjusted
translate_hierarchy_with_id <- function (hierarchy, dim_id_label, lang = "en") {
  if (!inherits(dim_id_label, "data.frame")) {
      dim_id_label <- swissdata:::list_to_dim_id_label(dim_id_label)
  }
  z <- hierarchy
  dims <- names(hierarchy)
  stopifnot("dimnames" %in% dim_id_label$dim)
  not_found <- setdiff(dims, filter(dim_id_label, dim == "dimnames")$id)
  if (length(not_found) > 0) {
      stop("Some dims not found: ", paste(not_found, collapse = ", "))
  }
  for (.dim in dims) {
    labels <- swissdata:::all_list_names(hierarchy[[.dim]])
    dpl <- duplicated(labels)
    if (any(dpl)) {
        stop("duplicate labels in hierarchy are not allowed: ",
            paste(labels[dpl], collapse = ", "))
    }
    id_label <- select(filter(dim_id_label, dim == .dim),
        id, label = one_of(lang))
    swissdata:::all_list_names(z[[.dim]]) <- translate_id_label_with_id(labels,
        id_label)
  }
  .dim <- "dimnames"
  id_label <- select(filter(dim_id_label, dim == .dim), label = id,
    id = one_of(lang))

  names(z) <- translate_id_label_with_id(names(z), id_label)
  z
}

#' @export
list_to_dim_id_label <- function(labels){
  labels <- null_to_empty_string(labels)
  tibble(dim = names(labels), data = labels) %>%
    rowwise() %>%
    mutate(id = list(names(data)))  %>%
    mutate(data = list(lapply(data, as_tibble))) %>%
    unnest() %>%
    unnest()
}


# swissdata:::translate_id_label
translate_id_label_with_id <- function (x, id_label) {
  not_in_tbl <- setdiff(x, id_label$label)
  if (length(not_in_tbl) > 0) {
      stop("some ids are not defined in 'id_label': ", paste(not_in_tbl,
          collapse = ", "), call. = FALSE)
  }
  tibble(label = x) %>%
    left_join(id_label, by = "label") %>%
    mutate(text = paste0(label, " - ", id)) %>%
    pull(text)
}

