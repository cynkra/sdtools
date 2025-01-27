# sdtools

The `sdtools` R package provides functions that work with and modify [swissdata](https://github.com/mbannert/swissdata) objects (datasets).

## Functions ##

List of available function.

#### Reading and Writing ####

```r
dataset_read()         # reads swissdata object from file system (YAML or JSON)
dataset_read_s3()      # reads swissdata object from Amazon S3 bucket.

dataset_write()        # saves swissdata object to file system (YAML or JSON)
```

#### Printing and Validating ####

```r
str()                  # prints the sturcture of the swissdata hierarchy
print()                # prints top 5 rows of the data and the structure of hierarchy

dataset_validate()     # tests the validity of swissdata object
```

#### Manipulating Datasets ####

```r
dataset_merge()        # combines two swissdata objects into one
```

#### Manipulating Dimensions, Levels, and Hierarchy ####

```r
dim_add()              # adds a new dimension to the data
dim_drop()             # removes the dimension from the data
dim_rename()           # renames the dimension

level_drop()           # removes a level from the specified dimension
level_rename()         # renames a level in the specified dimension

hierarchy_add()        # adds a new level to the hierarchy
hierarchy_move()       # moves an existing level to a new parent
hierarchy_reorder()    # reorders the levels under specified parent
```

## Installation ##

Install from https://github.com/christophsax/sdtools or using remotes:

```r
remotes::install_github("christophsax/sdtools")
```

## Notes ##

Additional notes.

### Deviations from swissdata ###

Package has a few deviations from swissdata conventions that are still up for discussion:

- `_` instead of `.` in files and column names: https://github.com/mbannert/swissdata/issues/306
- time stamp as `updated_utc`, rather than `utc.updated`, because UTC is an attribute to `updated`, not the other way round.

### Reading from S3 ###

`sdtools` contains downloaders that read data from the regular runs.
This data is read from an open S3 bucket, and no authentification is required.
To start, simply do:

```r
sdtools::dataset_read_s3("ch_adecco_sjmi")
```

`swissdata` is run twice on a server and the results are stored on a S3 bucket.
A status report gives an overview on the scraping success and is available [HERE](https://sos-ch-dk-2.exo.io/swissdata/status.html)

