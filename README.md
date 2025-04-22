# creelutils <a href="https://wdfw-fp.github.io/creelutils/"><img src="man/figures/logo.png" align="right" height="126" alt="creelutils website" /></a>

  <!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
  [![R-CMD-check](https://github.com/wdfw-fp/creelutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wdfw-fp/creelutils/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

A R package for working with freshwater recreational creel data. This package contains a variety of utility functions which are considered general use and might be applicable in various places of a workflow. It also contains functions which perform a utility process; for example, the group of functions relating to the extract, transform, and load (ETL) process for uploading model estimates to the creel database.

## Installation

`creelutils` can be installed from GitHub with the `devtools` package. As GitHub is a source code repository, this requires the installation of [Rtools](https://cran.r-project.org/bin/windows/Rtools/). One day it may be distributed as a binary hosted on CRAN.

``` r
devtools::install_github("wdfw-fp/creelutils")
```
