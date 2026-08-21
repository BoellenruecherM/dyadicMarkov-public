# Partial bivariate pattern identification by AIC

Compares the partial bivariate patterns B1, B2, and B3 using AIC and
returns the selected pattern.

## Usage

``` r
partialPattern(empirical)
```

## Arguments

- empirical:

  An empirical bivariate count matrix with 16 rows and 2 columns, as
  returned by
  [`countEmpBivariate`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/countEmpBivariate.md).

## Value

A list with class `c("dyadic_pattern", "list")` containing components
`aic` (a data frame with candidate patterns and AIC values), `pattern`
(the selected pattern label), and `call`. It remains usable as an
ordinary list.

## Details

Conditional on the partial bivariate case, the G-squared deviance is
computed for each B1, B2, and B3 candidate before calculating \$\$AIC =
G^2 + 2k,\$\$ where \$\$G^2 = 2 \sum\_{ij} O\_{ij} \log(O\_{ij} /
E\_{ij}).\$\$ The candidate with the smallest AIC is selected.

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
partialPattern(emp)
#> Dyadic interaction pattern
#> Pattern: partial actor only (B2)
```
