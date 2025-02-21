# creelutils

A R package for working with freshwater recreational creel data. This package contains a variety of utility functions which are considered general use and might be applicable in various places of a workflow. It also contains functions which perform a utility process; for example, the group of functions relating to the extract, transform, and load (ETL) process for uploading model estimates to the creel database.

## Installation

`creelutils` can be installed from GitHub with either the `devtools` or `remotes` packages. As GitHub is a source code repository, this requires the installation of [Rtools](https://cran.r-project.org/bin/windows/Rtools/). One day it may be distributed as a binary hosted on CRAN.

``` r
devtools::install_github("wdfw-fp/creelreview")

# Alternatively 
remotes::install_github("wdfw-fp/creelreview")
```

## Overview

Getting access to data:

These functions interact with Washington's public data warehouse, data.wa.gov. This website serves as a read-only interface to the internal PostgreSQL database.

-   fetch_dwg - Download freshwater recreational fishery creel datasets from . Requires a "fishery_name" field (e.g., Skagit winter steelhead 2021).

Internal use functions:

These functions interact directly with the WDFW PostgreSQL creel database, which requires proper credentials and permissions.

-   establish_db_con - Connect to the creel database. Requires a local `config.yml` file and an active username/password.

-   fetch_db_table - Send queries to the creel database as R code that is translated into SQL by the `dbplyr` package.
