# Generate QAQC report

Renders a HTML report on data quality assurance/quality control (QAQC)
for one or more freshwater creel fisheries and saves them to a specified
directory.

## Usage

``` r
generate_report(
  fishery_names,
  output_dir =
    "T:/DFW-Team FP FW Creel Monitoring Program - General/Project_Support_Files/Data Quality Assurance/QAQC Reports"
)
```

## Arguments

- fishery_names:

  A character vector of freshwater creel fishery names.

- output_dir:

  Path to the directory where rendered HTML reports will be saved.
  Defaults to the standard WDFW Teams QAQC Reports folder. On Windows,
  mapped drives are supported (e.g., T:/path/).

## Value

Invisibly returns a summary data frame of render results.

## Details

**Note - VPN Required:** By default reports are written to a mapped
network "T:/" drive. You must be either directly connected to the
internal network or through VPN for reports to save in this default
location.

## Examples

``` r
if (FALSE) { # \dontrun{
# Single fishery, default output location
generate_report("Hoh winter steelhead 2025-26")

# Multiple fisheries, custom output location
generate_report(
  fishery_names = c("Skagit fall salmon 2025", "Chehalis winter steelhead 2025-26"),
  output_dir = "<path>"
)
} # }
```
