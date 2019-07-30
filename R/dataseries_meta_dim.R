#' @export
dataseries_meta_dim <- function(x) {

  meta <- x$meta
  set_id <- meta$set_id
  meta_dim_one <- function(dim_name) {

    if (!(dim_name %in% names(meta$labels))) {
      stop("invalid dim_name: ", dim_name)
    }

    lookup_dim_class <- function(x) {
      ul <- unlist(meta$dataseries$map)
      gsub("\\d$", "", names(ul))[match(x, ul)]
    }

    lookup_units <- function(dim_name) {

      # outdated way
      if (is.null(names(meta$units))) {
        if (dim_name == unname(unlist(meta$dataseries$map))[1]){
          z <- meta$units[[1]]$en
          if (is.null(z)) {
            z <- "missing unit"
          }
          message("incorrect or outdated unit specification in ", set_id)

        } else {
          z <- ""
        }
      # if 'all', use unit labels for the first dim only.
      } else if (identical(names(meta$units), "all")) {
        if (dim_name == unname(unlist(meta$dataseries$map))[1]){
          z <- meta$units[[1]]$en
          if (is.null(z)) {
            z <- "missing unit"
          }
        } else {
          z <- ""
        }
      } else if (dim_name %in% names(meta$units)) {
        z <- sapply(meta$units[[dim_name]], function(e) e$en)
      } else {
        z <- ""
      }

      if (is.null(z)) return("")
      z
    }

    lookup_aggregate <- function(x) {
      if (is.null(names(meta$aggregate))) {
        z <- meta$aggregate[[1]]
      } else {
        z <- meta$aggregate[[x]]
      }
      if (is.null(z)) return("")
      z

    }



    if (dim_name %in% names(meta$hierarchy)) {
      structure <-
        yaml_to_structure(
          rapply(meta$hierarchy[[dim_name]], function(x) x, how = "list")
        )
    } else {
      structure <- names(meta$labels[[dim_name]])
    }



    dim_id <- gsub("^.*- ", "", structure)

    english_if_possible <- function(e) {
      z <- e$en
      if (is.null(z)) z <- e$de
      z
    }

    label_list <- unlist(lapply(meta$labels[[dim_name]], english_if_possible))
    label = unname(label_list[dim_id])

    dim_pos = match(dim_name, unname(unlist(meta$dataseries$map)))
    dim_class <- lookup_dim_class(dim_name)


    css_class <- character(length(structure))

    # add correct css class to split insatances
    if (dim_class == "split") {
      # default: selectable
      css_class[] <- "split-selectable"
      # non selectable nodes
      css_class[dim_id %in% meta$dataseries$`non-selectable`] <- ""
      # slected 'main' series
      css_class[dim_id %in% meta$dataseries$selected] <- "split-selectable split-selected"
    }

    data.table(
      var = gsub("\\.", "_", set_id),
      dim_pos = dim_pos,
      dim_id = dim_id,
      id = paste(gsub("\\.", "_", set_id), dim_pos, dim_id, sep = "."),
      dim_class = dim_class,
      structure = structure,
      view_pos = seq_along(label),
      label = label,
      css_class = css_class,
      unit = lookup_units(dim_name),
      conversion = lookup_aggregate(dim_name)
    )
  }

  dimdrop <- meta$dataseries$map[["drop"]]

  all_dims <- setdiff(unname(unlist(meta$dataseries$map)), dimdrop)
  rbindlist(lapply(all_dims, meta_dim_one))

}
