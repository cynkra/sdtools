#' @export
dataseries_DT <- function(x) {


  set_id <- x$meta$set_id
  message(set_id)

  meta <- x$meta
  data <- x$data

  # optional dimension droping: uses first instance in meta. If not what you
  # want, redefine ins user.yaml


  dimdrop <- meta$dataseries$map[["drop"]]

  if (!is.null(dimdrop)) {

    if (length(dimdrop) > 1) stop("todo: add multi dim support for drop")

    # instance that is used in droped dimension
    dimdrop.instance <- names(meta$labels[[dimdrop]])[1]

    expr <- parse(text = paste0(dimdrop, " == '", dimdrop.instance, "'"))

    dimdrop.name <- as.name(dimdrop)
    expr <- bquote(.(dimdrop.name) == .(dimdrop.instance))

    data <- data[eval(expr)]

    data[[dimdrop]] <- NULL

  }


  id.vars <- setdiff(names(data), c("date", "value"))

  # use order from 'meta$map'
  id.vars.ordered <- setdiff(unname(unlist(meta$dataseries$map)), dimdrop)
  stopifnot(identical(sort(id.vars), sort(id.vars.ordered)))

  id_underscore <- gsub("\\.", "_", set_id)

  DT <- tidyr::unite(data, "id", id.vars.ordered, sep = ".") %>%
    mutate(id = paste0(id_underscore, ".", id)) %>%
    as.data.table()

  setnames(DT, "date", "time")
  DT

}




