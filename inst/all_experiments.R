library(tidyverse)
library(sdtools)
library(dstools)  # dataseries_run()



path <- normalizePath(swissdata::path_wd())


# all experiments --------------------------------------------------------------

problem_baeren <-   c(
  "ch.fso.gdp.lngts",         # date column not in YEAR-MM-DD form
  "ch.fso.hesta",             # data meta mismatch
  "ch.fso.hesta.regions",     # data meta mismatch
  "ch.fso.na.prod.inst",      # data meta mismatch
  "ch.fso.pgai.idx_2015.det", # no data
  "ch.fso.prdctvty.idx1991",  # duplicate series

  # Ok, but slow
  "ch.fso.cah.edc",
  "ch.fso.cah.inv",
  "ch.fso.frv",
  "ch.fso.ggs",
  "ch.snb.devwkibiid",

  # show an error in prepare_dataseries TODO

  # time column needs to specified as the first date of the period
  "ch.fso.gdp.pa",
  "ch.fso.inc.idx1939",
  "ch.fso.na.constr.build",
  "ch.fso.na.gdp.use",
  "ch.fso.na.prod.sec",
  "ch.fso.nominc.qest",
  # series has no regular pattern
  "ch.fso.unemp",
  "ch.fso.unemp.rate"
)

all <- file.path(path, setdiff(
  list.files(path),
  problem_baeren
))

# not interesting
all <- grep(".fso.na", all, invert = TRUE, value = TRUE)


ans <- lapply(all, function(e) read_swissdata(e))
names(ans) <- basename(all)

# ans2 <- lapply(ans, function(e) try(dataseries_prepare(e)))
# names(ans2) <- names(ans)

names(ans)
dataseries_run(ans[[10]])




ch_fso_es_noga






x <- read_swissdata(path, "ch.snb.ambeschkla")

x$meta$hierarchy <- yaml::yaml.load('
hierarchy:
  d0:
    vt: ~
    v: ~
    t: ~
    iv: ~
  d1:
    t0:
      t1:
        vghw: ~
        bb: ~
      t2:
        hirk: ~
        gbg: ~
        vl: ~
        ef: ~
        v: ~
        gw: ~
        eu: ~
        gs: ~
        ed: ~
        ov: ~
')
x$meta$dataseries <- yaml::yaml.load('
map:
  split: d1
  var: d0
selected: t0
category: New Series
')
dataseries_run(x)

# str(x)




# str(x)
# z <- dataseries_prepare(x)


# x$meta$units <- list(all = list(en = " "))



# # ch_snb_amarbma




# originally planned:


# # write
# save_swissdata(x, type = "json")  # csv, json
# save_px(x)         # px
# save_excel(x)      # excel, translated data in first sheet, meta in second sheet

# # extractors
# get_data_translated(x, lang = "en")
# get_hierarchy_translated(x, lang = "en")

# # manipulators
# meta_replace(x, title = list(en = "Job vacancies by region"))
# meta_add(x, title = list(en = "Job vacancies by region"))





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




