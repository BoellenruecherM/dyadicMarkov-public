# Bivariate case identification for dyadic Markov chains

Identifies the bivariate case as `"trivial"`, `"univariate"`,
`"partial"`, or `"complete"` using two likelihood-ratio tests against
constrained bivariate structures.

## Usage

``` r
bivariateCase(empirical, alpha = 0.05)
```

## Arguments

- empirical:

  An empirical bivariate count matrix with 16 rows and 2

- alpha:

  A single number in (0, 1) giving the significance level. Default is
  0.05.

## Value

A list with class `c("dyadic_case", "list")` containing components
`testUnivariate`, `testPartial`, `case`, and metadata fields `alpha` and
`call`. It remains usable as an ordinary list.

## Details

The returned case corresponds to the global approach of the bivariate
method. It determines whether the sequence analyzed is treated as
trivial, univariate, partial bivariate, or complete bivariate before the
local identification of the pattern of interaction.

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
bivariateCase(emp, alpha = 0.05)
#> Bivariate dyadic case
#> Case: trivial
#> Alpha: 0.05
```
