path <- normalizePath(swissdata::path_wd())

# separate folders with multiple entries ---------------------------------------

# SWISSDATA FIXME: alle ch.fso.best.outlook.geo shoul not contain 2 files

all <- list.files(path, full.names = TRUE)
invalid <- all[sapply(sapply(all, list.files), length) > 3]
# invalid
# [1] "/Users/christoph/git/swissdata/wd//ch.fso.besta.outlook"
# [2] "/Users/christoph/git/swissdata/wd//ch.fso.na.soa"
# [3] "/Users/christoph/git/swissdata/wd//ch.fso.na.soa.chg"

separate_folders <- function(x) {
  # multiple set ids
  set_ids <- gsub(".yaml", "", list.files(x, pattern = "yaml$"), fixed = TRUE)

  # create folder
  lapply(file.path(path, set_ids), ensure_path)

  # copy files to these folders
  copy_set_id <- function(set_id) {
    file.copy(
      list.files(x, pattern = set_id, full.names = TRUE),
      to = file.path(path, set_id)
    )
  }
  lapply(set_ids, copy_set_id)

  # remove existing dir
  unlink(x, recursive = TRUE)
}

if (length(invalid > 0)) lapply(invalid, separate_folders)

