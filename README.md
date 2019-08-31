# sdtools

The `sdtools` R package summarizes tools that work with swissdata datasets. This is work in progress. If this is a little cleaner, it is planned to open source it.

I am doing a few deviations from swissdata conventions that are still up for discussion:

- I am using `_` instead of `.` in files and column names: https://github.com/mbannert/swissdata/issues/306

- Time stamp as `updated_utc`, rather than `utc.updated`, because UTC is an attribute to `updated`, not the other way round...

Install from:

https://github.com/christophsax/sdtools

sdtools are used to work with and to modify swissdata objects (datasets).

Basically, it allows you to read datasets (data and meta), to manipulate and merge (`marry()` them. In order to merge files, the dimensions and the labels need to be aligned, which can be done by using `relabel`, `dim_add`, `dim_drop` or manually adjust the `x$meta` list. `dstools::dataseries_run()` visualizes the datasets in the dataseries shiny app. `str()` should be useful as well.

sdtools contains downloaders that read data from the regular runs. This data is read from an open S3 bucket, and no authentification is required. To start, simply do:

```r
sdtools::read_swissdata_s3("ch_comb_vacancies")
```

swissdata is run twice on a server and the results are stored on a S3 bucket. A status report gives an overview on the scraping success and is available at:

https://sos-ch-dk-2.exo.io/swissdata/status.html
