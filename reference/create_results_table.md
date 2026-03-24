# create_results_table

This helper function creates a results table for each QAQC check.

## Usage

``` r
create_results_table(pass, critical, qaqc_check_type, error_count, message)
```

## Arguments

- pass:

  logical / TRUE if the check passed, FALSE if it failed

- critical:

  logical / TRUE if the check is critical, FALSE if it is not

- qaqc_check_type:

  character / type of QAQC check

- error_count:

  numeric / number of errors found for a given check

- message:

  character / message to display in results table

## Value

template results table
