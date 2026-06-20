# Univariate pattern identification for dyadic Markov chains

Computes empirical transition counts, estimates transition probabilities
by maximum likelihood, and performs likelihood-ratio tests against the
actor-only and partner-only constrained models to identify the
univariate pattern of interaction.

## Usage

``` r
univariatePattern(chainFM, chainSM, states, alpha = 0.05)
```

## Arguments

- chainFM:

  Vector of observed states for the first member (FM).

- chainSM:

  Vector of observed states for the second member (SM).

- states:

  A single integer \>= 2 giving the number of states.

- alpha:

  A single number in (0, 1) giving the significance level. Default is
  0.05.

## Value

A list with class `c("dyadic_pattern", "list")` containing two `htest`
objects (`TEST.AM`, `TEST.PM`), a string `pattern`, and metadata fields
`alpha`, `states`, and `call`. It remains usable as an ordinary list.

## Details

Pattern labels summarize which structure is retained by the tests:
`IM (A0)` denotes an independence pattern, `APM (A1)` an actor-partner
pattern, `AM (A2)` an actor-only pattern, and `PM (A3)` a partner-only
pattern.

## Examples

``` r
chainFM <- c(1L, 2L, 1L, 2L, 2L, 1L)
chainSM <- c(2L, 1L, 2L, 1L, 1L, 2L)
univariatePattern(chainFM, chainSM, states = 2L, alpha = 0.05)
#> Dyadic interaction pattern
#> Pattern: IM (A0)
#> Alpha: 0.05
#> States: 2
```
