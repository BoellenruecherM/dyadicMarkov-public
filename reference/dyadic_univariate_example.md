# Synthetic univariate dyadic sequence example

A synthetic dyadic sequence with 90 observations, designed for package
workflow examples.

## Usage

``` r
dyadic_univariate_example
```

## Format

A data frame with 90 rows and 3 columns:

- time:

  Index of the measurement occasion.

- FM:

  Integer state for the first member, taking values 1 or 2.

- SM:

  Integer state for the second member, taking values 1 or 2.

## Details

The package workflow classifies this example as `PM (A3)` using
[`univariatePattern`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/univariatePattern.md)
with `states = 2`.
