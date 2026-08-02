# Empirical transition counts for bivariate dyadic sequences

Computes empirical transition counts for bivariate categorical dyadic
sequences with two variables. This function currently supports
`states = 2` only.

## Usage

``` r
countEmpBivariate(chainFM_V1, chainSM_V1, chainFM_V2, chainSM_V2, states = 2L)
```

## Arguments

- chainFM_V1, chainSM_V1:

  Vectors of observed states for variable 1 for the first and second
  member.

- chainFM_V2, chainSM_V2:

  Vectors of observed states for variable 2 for the first and second
  member.

- states:

  A single integer. Currently only `2` is supported. Default is 2.

## Value

An integer matrix with class `c("dyadic_counts", "matrix", "array")`
with 16 rows and 2 columns when `states = 2`. It remains usable as an
ordinary matrix.

## Details

The bivariate counter currently supports `states = 2` only. Rows
represent the previous dyadic states of variable 1 and variable 2.
Writing \\K\\ for the number of states, the row index at time \\t\\ is
\$\$r_t = 1 + K^2 \[K(FM\_{V1,t} - 1) + (SM\_{V1,t} - 1)\] +
K(FM\_{V2,t} - 1) + (SM\_{V2,t} - 1)\$\$ Columns correspond to the state
of the first member on variable 1 at the next time point,
\\FM\_{V1,t+1}\\.

## Examples

``` r
chainFM_V1 <- c(1L, 2L, 1L, 2L, 2L, 1L)
chainSM_V1 <- c(2L, 1L, 2L, 1L, 1L, 2L)
chainFM_V2 <- c(1L, 1L, 2L, 2L, 1L, 2L)
chainSM_V2 <- c(2L, 2L, 1L, 1L, 2L, 1L)
emp <- countEmpBivariate(
  chainFM_V1, chainSM_V1, chainFM_V2, chainSM_V2,
  states = 2L
)
dim(emp)
#> [1] 16  2
```
