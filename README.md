# creelreview

## Overview

`creelreview` is a R package for performing rigorous, standardized data quality assurance and quality control checks (QAQC) on freshwater recreational creel data. It is used during in-season monitoring of ongoing fisheries and for critical review of complete datasets during the post-season. This work supports [CreelEstimates](https://github.com/wdfw-fp/CreelEstimates).

### High level functions

-   run_all_checks.R
-   get_grading.R
-   generate_report.R

### Low level functions

-   assumption_checks.R
-   effort_checks.R
-   interview_checks.R
-   catch_checks.R

### Forms

-   season_review_checks.R

## Installation

`creelreview` can be installed from source code with either the `devtools` or `remotes` packages:

``` r
devtools::install_github("wdfw-fp/creelreview")

# Alternatively 
remotes::install_github("wdfw-fp/creelreview")
```
