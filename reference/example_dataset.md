# Example creel dataset

This dataset is a copy of the "Skagit winter steelhead 2021" fishery. It
was downloaded on January 22, 2025 from the public data repository
data.wa.gov. Hosted there are views from WDFW's internal creel database,
which are formatted queries of primary data components (e.g., effort,
interviews, and catch).

## Usage

``` r
example_dataset
```

## Format

An object of class `list` of length 6.

## Details

@format A list with the following data frames:

- effort:

  placeholder with columns `count_sequence`, and etc.

@examples summary(example_dataset) head(example_dataset\$effort)
