# Complete bivariate pattern identification by AIC

Compares the complete bivariate patterns C, D1–D4, and E1–E4 using AIC
and returns the selected pattern.

## Usage

``` r
completePattern(empirical)
```

## Arguments

- empirical:

  An empirical bivariate count matrix with 16 rows and 2 columns, as
  returned by
  [`countEmpBivariate`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/countEmpBivariate.md).

## Value

A list with class `c("dyadic_pattern", "list")` containing components
`aic` (a data frame with columns `pattern`, `matrix`, and `aic`),
`pattern` (the selected pattern label), and `call`. It remains usable as
an ordinary list.

## Details

Conditional on the complete bivariate case, AIC is used to select among
the C, D1–D4, and E1–E4 structures.

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
completePattern(emp)
#> Dyadic interaction pattern
#> Pattern: complete partner on both (E1)
```
