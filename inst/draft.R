library(tidyverse)
library(sdtools)
library(dstools)  # dataseries_run()




# ch_comb_jobs -----------------------------------------------------------------

x0 <- read_swissdata("/Users/christoph/git/swissdata/wd/ch.fso.besta.sex")
y0 <- read_swissdata("/Users/christoph/git/swissdata/wd/ch.fso.besta")

x <- relabel(x0,
  "employment_rate" = "occupancy",
  "sector.tot" = "sector.596",
  "sector.2"   = "sector.543",
  "sector.3"   = "sector.4596"
)

y <- relabel(y0,
  "section" = "sector"
) %>%
dim_add(dim = "geo", level = "0")

z <- marry(x, y)

z$set_id <- "ch_comb_jobs"

z$meta$title$en <- "Jobs"
z$meta$dataseries <- yaml::yaml.load('
map:
  split: sector
  select:
  - occupancy
  - sex
  - geo
selected: "596"
category: Labor Market
')

dataseries_run(z)


# ch_comb_vacancies --------------------------------------------------------------

x0 <- read_swissdata("/Users/christoph/git/swissdata/wd/ch.fso.besta.mjr")
y0 <- read_swissdata("/Users/christoph/git/swissdata/wd/ch.fso.besta.vacancies")

# SWISSDATA FIXME
x0$meta$units <- yaml::yaml.load('
type:
  1:
    en: Job vacancies
  2:
    en: "Index (2nd quarter 2015 = 100)"
  3:
    en: "Percent"
'
)

x <- relabel(x0,
  "sector.tot" = "sector.596",
  "sector.2"   = "sector.543",
  "sector.3"   = "sector.4596",
  "unit"       = "type"
)
y <- dim_add(y0, dim = "geo", level = "0")

z <- marry(x, y)
# str(z)

z$meta$title$en <- "Job vacancies"
z$set_id <- "ch_comb_vacancies"
z$meta$dataseries <- yaml::yaml.load('
map:
  split: sector
  select:
  - type
  - geo
selected: "596"
category: Labor Market
')

dataseries_run(z)


# write_swissdata(z, path = "~/Desktop/ch.comb.vacancies")




# GDP --------------------------------------------------------------------------

x <- read_swissdata_json("/Users/christoph/git/seco/dataexport")

x$meta$dataseries <- yaml::yaml.load('
map:
  split: structure
  select:
  - price_adj
  - seas_adj
selected: gdp
category: General Economy
')

dataseries_run(x)













x <- read_swissdata("/Users/christoph/git/swissdata/swissdata/wd/ch.fso.besta.mjr")


# write
save_swissdata(x, type = "json")  # csv, json
save_px(x)         # px
save_excel(x)      # excel, translated data in first sheet, meta in second sheet

# extractors
get_data_translated(x, lang = "en")
get_hierarchy_translated(x, lang = "en")

# manipulators
meta_replace(x, title = list(en = "Job vacancies by region"))
meta_add(x, title = list(en = "Job vacancies by region"))


# wie kann ich schnell ein plot machen?

meta_add(x, '
dataseries:
  map:
    split: idx_type
  selected: sch
  category: Labor Market
')

# recodeing dimnames and instances (ensures that adjustments are performed everywhere)



# library(swissdata)
# set_update("ch.fso.besta.mjr")
# set_update("ch.fso.besta.vacancies")










.DT <- dataseries_DT(z)
.meta_dim <- dataseries_meta_dim(z)
.meta_main <- dataseries_meta_main(z)

source("/Users/christoph/Desktop/run_dataseries.R")











.meta_main <- rbindlist(lapply(sids, swissdata_meta_main))




meta <- z$meta









