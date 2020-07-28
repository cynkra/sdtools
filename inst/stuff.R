
library(jsonlite)
url_json <- "https://www.seco.admin.ch/dam/seco/de/dokumente/Wirtschaft/Wirtschaftslage/BIP_Daten/ch_seco_gdp_json.txt.download.txt/ch_seco_gdp_json.txt"
url_csv <- "https://www.seco.admin.ch/dam/seco/de/dokumente/Wirtschaft/Wirtschaftslage/BIP_Daten/ch_seco_gdp_csv.csv.download.csv/ch_seco_gdp.csv"


# dataset_download(url, url_csv = NULL, url_json = NULL)

seco <- dataset_create(
  read_csv(url_csv),
  read_json(url_json),
  dataset_id = "ch_seco_gdp"
)

tbl_labels <- flat_labels(seco$meta$labels)


# flat sceleton
dims <- setdiff(names(seco$data), c("date", "value"))
placeholder <- ""
tbl_labels_skeleton <-
  seco$data %>%
  select(!! dims) %>%
  distinct() %>%
  pivot_longer(cols = dims, names_to = "dim", values_to = "id") %>%
  arrange(dim) %>%
  distinct() %>%
  crossing(lang) %>%
  # mutate(value = id) %>%
  mutate(value = placeholder) %>%
  pivot_wider(names_from = lang, values_from = value)



unflat_labels <- function(tbl_labels) {

}



names(seco$meta)


names(seco$meta$aggregate)


