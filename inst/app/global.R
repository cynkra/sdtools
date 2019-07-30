# source("/Users/christoph/git/dsdb/collect.R")


# source('/Users/christoph/git/embed/dg.R')

# Need to be set to TRUE for test server, FALSE for production
test.mode <- FALSE


# make sure you have the same wd as on server
if (version$os != "linux-gnu"){
  # Sys.setenv(DSDB_PATH = "~/git/dsdb")
  Sys.setenv(DSDB_PATH = "~/.dataseries")
} else {
  Sys.setenv(DSDB_PATH = "~/.dataseries")
  # if (test.mode) {
  #   Sys.setenv(DSDB_PATH = "~/dsdb-dev")
  # }
}

# sapply(list.files("functions", full.names=TRUE), source)


# install.packages("shiny", "xts", "data.table", "dygraphs")

library(shiny)
library(ggplot2)
library(extrafont)



library(xts)
library(data.table)

library(dygraphs)
library(dstools)

if (!test.mode){  # no need to read this twice
  load(file.path(Sys.getenv("DSDB_PATH") , "dta.RData"))
  setkey(.DT, id, time)
  setkey(.meta_dim, id)

  setkey(.meta_main, var)
  setkey(.meta_series, id)
  setkey(.meta_disabled, id)
}










