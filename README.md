<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/wdfw-fp/creelreview/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wdfw-fp/creelreview/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# creelreview

## Overview

`creelreview` is a R package for performing rigorous, standardized data quality assurance and quality control checks (QAQC) on freshwater recreational creel data. It is used during in-season monitoring of ongoing fisheries and for critical review of complete datasets during the post-season. This work supports [CreelEstimates](https://github.com/wdfw-fp/CreelEstimates).


### Data Review Architecture

The standardized data review process consists of three layers. The low-level check functions accept inputs from `creelutils::fetch_dwg()` and return a result table with a single row per check. Each data check has a clear name that describes the domain and what is checked (e.g., `interview_na.fishing.location()`). 


#### LOW: Individual check functions

"Does this specific expectation hold?"

 - `effort_*()`, `interview_*()`, `catch_*()`
 
#### MID: Perform all checks on a given dataset

"Is this dataset ready for a model run?"

 - `run_all_checks()`
 
#### HIGH: Perform assessment across multiple fisheries

"Which datasets need attention?"

 - `assess_datasets()`
 

## Installation

`creelreview` can be installed from GitHub with the `remotes` package.

``` r
remotes::install_github("wdfw-fp/creelreview")
```

### Prerequisites and Troubleshooting

- As GitHub is a source code repository, installation requires [Rtools](https://cran.r-project.org/bin/windows/Rtools/). To verify your C++ toolchain is functional, run `pkgbuild::has_build_tools(debug = TRUE)`. If it returns `TRUE`, Rtools is properly configured.

- Netskope can interfere with secure connections to GitHub during source package downloads. If installation fails with SSL certificate or "cannot open URL" errors, try disconnecting from the VPN.

- To force a fresh reinstall when `remotes` reports the package is already up to date:

``` r
# unload if currently loaded (safe to skip if not)
detach("package:creelutils", unload = TRUE)

# remove and reinstall
remove.packages("creelutils")
remotes::install_github("wdfw-fp/creelutils", force = TRUE)
```
