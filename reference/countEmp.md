# Empirical transition counts for univariate dyadic sequences

Computes empirical transition counts for the sequences of the first and
second member. Rows correspond to dyadic states of the two members, and
columns correspond to the state of the first member at the next time
point.

## Usage

``` r
countEmp(chainFM, chainSM, states)
```

## Arguments

- chainFM:

  Numeric vector of integer-valued observed states for the first member
  (FM). Must have the same length as `chainSM`.

- chainSM:

  Numeric vector of integer-valued observed states for the second member
  (SM). Must have the same length as `chainFM`.

- states:

  A single integer \>= 2 giving the number of states.

## Value

An integer matrix with class `c("dyadic_counts", "matrix", "array")`,
with \\states^2\\ rows and `states` columns. It remains usable as an
ordinary matrix. [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`utils::toLatex()`](https://rdrr.io/r/utils/toLatex.html) methods are
available for this result.

## Details

Rows correspond to current dyadic states \\(FM_t, SM_t)\\. For general
`states`, the row index is computed as
`1 + states * (FM_t - 1) + (SM_t - 1)`. Columns correspond to the state
of the first member at the next time point, \\FM\_{t+1}\\.

## Examples

``` r
chainFM <- c(1L, 2L, 1L, 2L, 2L, 1L)
chainSM <- c(2L, 1L, 2L, 1L, 1L, 2L)
countEmp(chainFM, chainSM, states = 2L)
#>         next_1 next_2
#> FM1_SM1 0      0     
#> FM1_SM2 0      2     
#> FM2_SM1 2      1     
#> FM2_SM2 0      0     
```
