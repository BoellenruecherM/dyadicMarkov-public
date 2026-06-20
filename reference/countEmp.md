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

  Vector of observed states for the first member (FM).

- chainSM:

  Vector of observed states for the second member (SM).

- states:

  A single integer \>= 2 giving the number of states.

## Value

An integer matrix with class `c("dyadic_counts", "matrix", "array")`,
with \\states^2\\ rows and `states` columns. It remains usable as an
ordinary matrix.

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
#>                   next_1 next_2
#> focal_1_partner_1 0      0     
#> focal_1_partner_2 0      2     
#> focal_2_partner_1 2      1     
#> focal_2_partner_2 0      0     
```
