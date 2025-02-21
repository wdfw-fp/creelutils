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

### Getting access to data:

These functions interact with Washington's public data warehouse, data.wa.gov. This website serves as a read-only interface to the internal PostgreSQL database.

-   `fetch_dwg` - Download freshwater recreational fishery creel datasets from . Requires a "fishery_name" field (e.g., Skagit winter steelhead 2021).

-   `query_creel_website` -

-   `get_fishery_data` -

### Internal use functions:

These functions interact directly with the WDFW PostgreSQL creel database, which requires proper credentials and permissions.

-   `establish_db_con` - Connect to the creel database. Requires a local 'config.yml' file and an active username/password.

-   `fetch_db_table` - Send queries to the creel database as R code that is translated into SQL by the dbplyr package.

#### ETL functions

This group of functions works with `CreelEstimates` to standardize and transform model output formatting, save analysis metadata, and write catch and effort model estimates to the creel database.

-   `process_estimates_bss` - Standardize and transform outputs from the 'Bayesian State-Space (BSS)' model to common format.

-   `process_estimates_pe` - Standardize and transform outputs from the 'Point Estimate (PE)' model to common format.

-   `map_data_grade` - Applies a user-provided "Provisional" or "Approved" data grade label to everything for a given set of model estimates. Uploading "Approved" estimates required restricted permissions.

-   `json_conversion` - Converts analysis metadata into JSON format and stores in an analysis lookup table linked by a session-specific id.

-   `transform_estimates` - Takes the outputs of `process_estimates_bss' and/or 'process_estimates_pe' and combines them into a single dataframe with some additional formatting. 

-   `prep_export` - If exporting model estimates to the creel database, this function queries relevant lookup tables in the database and joins uuid values with the creel estimates. This, for example, parses a 'catch_group' field (e.g., Coho_Adult_AD_Kept) into component fields with the appropriate database keys for values in each of the four component fields 'species_name', "life_stage_name", "fin_mark_desc, and 'fate_name'".

-   `export_estimates` - This is a wrapper function that calls a number of other sub-functions when writing estimates to the database.

    -   `write_analysis_lut` - Writes a new row (n = 1) to the model_analysis_lut table with a session-specific 'analysis_id' field that links this table with model_estimates_stratum and model_estimates_total tables in the database.

    -   `write_estimates_stratum` - Writes model estimates to the database at the finest grain available, retaining all levels of stratification.

    -   `write_estimates_total` - Writes model esimates to the database in a summarised format, which exludes several grouping variables such as 'period','section_num','angler_type', etc.

    -   `confirm_db_upload` - After model estimates are exported to the database, this function confirms that the model_analysis_lut now contains the unique 'analysis_id' value from the most recent session.
