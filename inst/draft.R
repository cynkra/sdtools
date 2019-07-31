library(tidyverse)
library(sdtools)
library(dstools)  # dataseries_run()

path <- normalizePath(swissdata::path_wd())
path_out <-  normalizePath(file.path(path, "..", "wd_dataseries"))

# ch_fso_hce_pu ----------------------------------------------------------------

x <- read_swissdata(path, "ch.fso.hce.pu")
x$meta$details <- NULL

# x$meta$labels$group$ac$en <- "Additional classifications"

x$meta$dataseries <- yaml::yaml.load('
map:
  split: structure
  select: nomchg
selected: tot
category: Consumption
')

# dataseries_run(z)
write_swissdata(z, path_out)



