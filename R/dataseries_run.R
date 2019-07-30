
#' @export
#' @import data.table
dataseries_run <- function(x) {

  x <- test_swissdata(x)

  if (!("set_id" %in% names(x$meta))) x$meta$set_id <- "set_id"

  .DT <<- dataseries_DT(x)
  .meta_dim <<- dataseries_meta_dim(x)
  .meta_main <<- dataseries_meta_main(x)

  # derived inputs
  .meta_allid <<- meta_allid()
  .meta_series <<- meta_series()
  .meta_class <<- meta_class()
  .meta_disabled <<- meta_disabled()

  # stuff previously in global.R

  .meta_dim_total <<- .meta_dim[structure == "Total"]
  .meta_dim_pos_n <<- .meta_dim[, .(pos_n = length(unique(dim_pos))), by = var]

  .meta_allid <<- unique(.DT[, id])

  setkey(.DT, id, time)
  setkey(.meta_dim, id)

  setkey(.meta_main, var)
  setkey(.meta_series, id)
  setkey(.meta_disabled, id)

  allvars <- unique(.meta_dim$var)
  PRE2 <- lapply(allvars, html_all)
  names(PRE2) <- allvars

  # http://www.mulinblog.com/a-color-palette-optimized-for-data-visualization/
  .colors <- c(
    "#4D4D4D",
  "#5DA5DA",
  "#FAA43A",
  "#60BD68",
  "#F15854",
  "#B276B2",
  "#DECF3F",
  "#F17CB0",
  "#B2912F",
  "#4afff0", "#34bdcc", "#4f61a1", "#461e78", "#440a4f", "#c3fbc4",
  "#85f9d6", "#79c7ad", "#a6cc7a", "#dfff7b",
  "#8d7b88", "#4e414f", "#baadb5", "#2d2538", "#837a80", "#fff68f",
  "#800080", "#f8b1cc", "#c29bff", "#8d0808"
  )

  tdir <- ensure_path("~/.dataseries")
  # main data file
  save(.DT, .meta_dim, .meta_main, .meta_series, .meta_disabled, .meta_class,
    .meta_dim_total, .meta_dim_pos_n, .meta_allid, PRE2, .colors,
    file = file.path("~/.dataseries", "dta.RData"))


  shiny::runApp(system.file(package = "swissdatatools", "app"))
}

