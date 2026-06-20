# Synthetic bivariate dyadic sequence example

A synthetic bivariate dyadic sequence with 90 observations, designed for
package workflow examples.

## Usage

``` r
dyadic_bivariate_example
```

## Format

A data frame with 90 rows and 5 columns:

- time:

  Index of the measurement occasion.

- FM_V1:

  Integer variable 1 state for the first member, taking values 1 or 2.

- SM_V1:

  Integer variable 1 state for the second member, taking values 1 or 2.

- FM_V2:

  Integer variable 2 state for the first member, taking values 1 or 2.

- SM_V2:

  Integer variable 2 state for the second member, taking values 1 or 2.

## Details

The bivariate workflow classifies this example as `complete` using
[`bivariateCase`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/bivariateCase.md)
with `alpha = 0.05`. The complete bivariate pattern selected by
[`completePattern`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/completePattern.md)
is `D2`.
