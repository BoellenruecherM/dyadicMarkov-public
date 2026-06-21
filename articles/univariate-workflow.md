# Univariate dyadic workflow

## Overview

This vignette shows the univariate workflow implemented in
`dyadicMarkov`. In the univariate setting, one categorical variable is
observed repeatedly for the two members of a dyad. The workflow follows
the single-case method described in Bollenrücher et al. (2023):
empirical transition counts are computed from the dyadic sequence,
transition probabilities are estimated by maximum likelihood, and
restricted actor/partner structures are compared to identify the pattern
of interaction.

The example uses the data set `dyadic_univariate_example` included in
the package. The data are synthetic and are used only to illustrate the
required input structure and the package workflow.

## Data

The univariate example contains one categorical variable for the first
member (`FM`) and the second member (`SM`) of a dyad. Each row
corresponds to one measurement occasion. In the first analysis, `FM` is
the first member sequence analyzed and `SM` is the second member
sequence.

``` r

utils::data("dyadic_univariate_example", package = "dyadicMarkov")

head(dyadic_univariate_example)
#>   time FM SM
#> 1    1  2  1
#> 2    2  2  1
#> 3    3  1  2
#> 4    4  1  1
#> 5    5  1  1
#> 6    6  2  1
dim(dyadic_univariate_example)
#> [1] 90  3
```

The states are integer-coded. In this example, both members can take the
values 1 or 2.

``` r

table(dyadic_univariate_example$FM)
#> 
#>  1  2 
#> 50 40
table(dyadic_univariate_example$SM)
#> 
#>  1  2 
#> 47 43
```

## Empirical transition counts

The first step is to compute empirical transition counts with
[`countEmp()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/countEmp.md).
For `states = 2`, the rows represent the four possible previous dyadic
states `(FM_t, SM_t)`, and the columns represent the state of the first
member at the next time point, `FM_{t+1}`.

``` r

emp_uni <- dyadicMarkov::countEmp(
  chainFM = dyadic_univariate_example$FM,
  chainSM = dyadic_univariate_example$SM,
  states = 2L
)

emp_uni
#>         next_1 next_2
#> FM1_SM1 26     4     
#> FM1_SM2 5      14    
#> FM2_SM1 12     5     
#> FM2_SM2 7      16
class(emp_uni)
#> [1] "dyadic_counts" "matrix"        "array"
```

The resulting object keeps ordinary matrix behavior.

``` r

dim(emp_uni)
#> [1] 4 2
rowSums(emp_uni)
#> FM1_SM1 FM1_SM2 FM2_SM1 FM2_SM2 
#>      30      19      17      23
```

## Maximum-likelihood transition probabilities

The empirical counts can be converted into transition probabilities
using
[`mleEstimation()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/mleEstimation.md).
Rows with positive totals are normalized independently. For an
unobserved previous-state combination, the transition probabilities are
unidentified; the package returns a uniform row as an implementation
convention.

``` r

fit_uni <- dyadicMarkov::mleEstimation(emp_uni)

round(fit_uni, 3)
#>         next_1 next_2
#> FM1_SM1 0.867  0.133 
#> FM1_SM2 0.263  0.737 
#> FM2_SM1 0.706  0.294 
#> FM2_SM2 0.304  0.696
rowSums(fit_uni)
#> FM1_SM1 FM1_SM2 FM2_SM1 FM2_SM2 
#>       1       1       1       1
class(fit_uni)
#> [1] "dyadic_mle" "matrix"     "array"
```

The estimated transition matrix summarizes the empirical transition
structure of the observed dyadic sequence.

## Univariate pattern identification

The function
[`univariatePattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/univariatePattern.md)
performs the univariate identification step. It compares the
unrestricted actor-partner structure with actor-only and partner-only
restricted structures through likelihood-ratio tests. The two test
outcomes are then combined to classify the sequence as Actor-Partner,
Actor-only, Partner-only or Independence.

``` r

pat_uni <- dyadicMarkov::univariatePattern(
  chainFM = dyadic_univariate_example$FM,
  chainSM = dyadic_univariate_example$SM,
  states = 2L,
  alpha = 0.05
)

pat_uni
#> Dyadic interaction pattern
#> Pattern: PM (A3)
#> Alpha: 0.05
#> States: 2
```

For this example, the selected pattern is stored in the `pattern`
component.

``` r

pat_uni$pattern
#> [1] "PM (A3)"
pat_uni$TEST.AM
#> 
#>  Likelihood-ratio test, Actor-only model
#> 
#> data:  Observed vs Estimated
#> X-squared = 24.551, df = 2, p-value = 4.666e-06
#> alternative hypothesis: The unrestricted model fits the data better
pat_uni$TEST.PM
#> 
#>  Likelihood-ratio test, Partner-only model
#> 
#> data:  Observed vs Estimated
#> X-squared = 1.8984, df = 2, p-value = 0.3871
#> alternative hypothesis: The unrestricted model fits the data better
summary(pat_uni)
#> $pattern
#> [1] "PM (A3)"
#> 
#> $alpha
#> [1] 0.05
#> 
#> $states
#> [1] 2
#> 
#> $call
#> dyadicMarkov::univariatePattern(chainFM = dyadic_univariate_example$FM, 
#>     chainSM = dyadic_univariate_example$SM, states = 2L, alpha = 0.05)
#> 
#> attr(,"class")
#> [1] "summary_dyadic_pattern" "list"
```

The returned object is a list with an additional S3 class. It can
therefore be printed and summarized, while still allowing direct access
to its components.

## Interpretation

In this example, the selected pattern is `PM (A3)`. This indicates that
the previous state of the second member is retained in the restricted
structure, whereas the previous state of the first member is not
retained. In the terminology of the univariate method, this corresponds
to a partner-only pattern.

The result should be interpreted as a pattern description for the member
sequence analyzed by the function. The function call above analyzes `FM`
as the first member sequence and `SM` as the second member sequence.
Reversing the two arguments analyzes the sequence from the perspective
of the second member. Thus, describing both members of a dyad requires
two calls, and each returned pattern is specific to the member sequence
analyzed.

``` r

pat_uni_reverse <- dyadicMarkov::univariatePattern(
  chainFM = dyadic_univariate_example$SM,
  chainSM = dyadic_univariate_example$FM,
  states = 2L,
  alpha = 0.05
)

pat_uni_reverse
#> Dyadic interaction pattern
#> Pattern: PM (A3)
#> Alpha: 0.05
#> States: 2
```

## References

Bollenrücher, Mégane, Joëlle Darwiche, and Jean-Philippe Antonietti.
2023. “Dyadic Pattern Analysis Using Longitudinal Actor-Partner
Interdependence Model with Markov Chains for Unique Case Analysis.” *The
Quantitative Methods for Psychology* 19 (3): 230–43.
<https://doi.org/10.20982/tqmp.19.3.p230>.
