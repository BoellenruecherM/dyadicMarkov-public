# Bivariate dyadic workflow

## Overview

This vignette shows the bivariate workflow implemented in
`dyadicMarkov`. In the bivariate setting, two categorical variables are
observed repeatedly for the two members of a dyad. The bivariate method
follows the global-and-local procedure described in Böllenrücher et al.
(in press).

The current bivariate functions support `states = 2`. With two binary
variables observed for two members, the previous state is described by
four binary components. The empirical bivariate count matrix therefore
has 16 rows and 2 columns.

Although the data are synthetic, the four columns can be read like real
repeated observations from a dyad. For example, `V1` could represent one
coded behavior or response, and `V2` could represent a second coded
behavior or response observed at the same measurement occasions. The
columns `FM_V1` and `SM_V1` then describe the two members on the main
variable, while `FM_V2` and `SM_V2` describe the same two members on the
second variable.

The 16 rows arise because the previous state combines four binary lagged
components: first member on `V1`, second member on `V1`, first member on
`V2`, and second member on `V2`. With two possible states for each
component, this gives $`2^4 = 16`$ previous-state combinations. The 2
columns represent the possible next states of the member sequence
analyzed on the current main variable.

## Data

The example data set `dyadic_bivariate_example` contains two categorical
variables for the first member and the second member of a dyad. Each row
corresponds to one measurement occasion.

``` r

utils::data("dyadic_bivariate_example", package = "dyadicMarkov")

head(dyadic_bivariate_example)
#>   time FM_V1 SM_V1 FM_V2 SM_V2
#> 1    1     2     1     1     2
#> 2    2     2     1     2     1
#> 3    3     2     2     2     1
#> 4    4     2     2     2     2
#> 5    5     2     2     1     2
#> 6    6     2     2     1     1
dim(dyadic_bivariate_example)
#> [1] 90  5
```

The four chains are first-member and second-member sequences for the
main variable (`V1`) and the second variable (`V2`).

``` r

table(dyadic_bivariate_example$FM_V1)
#> 
#>  1  2 
#> 16 74
table(dyadic_bivariate_example$SM_V1)
#> 
#>  1  2 
#> 18 72
```

## Empirical bivariate transition counts

The first step is to construct the empirical bivariate transition count
matrix with
[`countEmpBivariate()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/countEmpBivariate.md).
Rows represent the 16 possible previous-state combinations of the two
dyadic variables. Columns represent the next state of the first member
on the main variable.

``` r

emp_bi <- dyadicMarkov::countEmpBivariate(
  chainFM_V1 = dyadic_bivariate_example$FM_V1,
  chainSM_V1 = dyadic_bivariate_example$SM_V1,
  chainFM_V2 = dyadic_bivariate_example$FM_V2,
  chainSM_V2 = dyadic_bivariate_example$SM_V2,
  states = 2L
)

emp_bi
#>                                     next_1 next_2
#> mainFM1_mainSM1_secondFM1_secondSM1 7      0     
#> mainFM1_mainSM1_secondFM1_secondSM2 0      2     
#> mainFM1_mainSM1_secondFM2_secondSM1 0      0     
#> mainFM1_mainSM1_secondFM2_secondSM2 1      0     
#> mainFM1_mainSM2_secondFM1_secondSM1 0      1     
#> mainFM1_mainSM2_secondFM1_secondSM2 0      1     
#> mainFM1_mainSM2_secondFM2_secondSM1 2      0     
#> mainFM1_mainSM2_secondFM2_secondSM2 0      2     
#> mainFM2_mainSM1_secondFM1_secondSM1 0      0     
#> mainFM2_mainSM1_secondFM1_secondSM2 2      1     
#> mainFM2_mainSM1_secondFM2_secondSM1 1      1     
#> mainFM2_mainSM1_secondFM2_secondSM2 0      3     
#> mainFM2_mainSM2_secondFM1_secondSM1 1      1     
#> mainFM2_mainSM2_secondFM1_secondSM2 1      4     
#> mainFM2_mainSM2_secondFM2_secondSM1 1      5     
#> mainFM2_mainSM2_secondFM2_secondSM2 0      52
class(emp_bi)
#> [1] "dyadic_counts" "matrix"        "array"
dim(emp_bi)
#> [1] 16  2
```

## Global bivariate case

The function
[`bivariateCase()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/bivariateCase.md)
performs the global step of the bivariate method. It applies the package
chi-squared comparisons to classify the dependence structure of the
member sequence analyzed as a trivial, univariate, partial bivariate or
complete bivariate case.

``` r

case_bi <- dyadicMarkov::bivariateCase(emp_bi, alpha = 0.05)

case_bi
#> Bivariate dyadic case
#> Case: complete
#> Alpha: 0.05
case_bi$case
#> [1] "complete"
summary(case_bi)
#> $case
#> [1] "complete"
#> 
#> $alpha
#> [1] 0.05
#> 
#> $call
#> dyadicMarkov::bivariateCase(empirical = emp_bi, alpha = 0.05)
#> 
#> attr(,"class")
#> [1] "summary_dyadic_case" "list"
```

This example is identified as a complete bivariate case. The appropriate
local step is therefore to compare complete bivariate candidate
patterns.

## Local pattern identification for a complete case

For a complete bivariate case,
[`completePattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/completePattern.md)
compares the complete bivariate candidate structures by AIC and returns
the selected pattern.

``` r

complete_bi <- dyadicMarkov::completePattern(emp_bi)

complete_bi
#> Dyadic interaction pattern
#> Pattern: complete actor on the main, actor partner on the second (D2)
complete_bi$pattern
#> [1] "complete actor on the main, actor partner on the second (D2)"
complete_bi$aic
#>                                                          pattern matrix
#> 1                             complete actor partner on both (C)      C
#> 2 complete partner on the main, actor partner on the second (D1)     D1
#> 3   complete actor on the main, actor partner on the second (D2)     D2
#> 4 complete actor partner on the main, partner on the second (D3)     D3
#> 5   complete actor partner on the main, actor on the second (D4)     D4
#> 6                                  complete partner on both (E1)     E1
#> 7         complete partner on the main, actor on the second (E2)     E2
#> 8         complete actor on the main, partner on the second (E3)     E3
#> 9                                    complete actor on both (E4)     E4
#>        aic
#> 1 32.00000
#> 2 30.03720
#> 3 28.42735
#> 4 33.33973
#> 5 38.60730
#> 6 30.55374
#> 7 42.91748
#> 8 36.47261
#> 9 40.57170
```

In this example, the selected complete bivariate pattern is `D2`,
labelled by the package as complete actor on the main, actor-partner on
the second.

## Reassigning member and main-variable roles

The previous analysis used `FM_V1` as the member-variable sequence
analyzed, `V1` as the main variable and `V2` as the second variable. In
the bivariate method, these are analysis roles rather than fixed
identities. To describe the dyad more completely, the roles can be
reassigned so that each member-variable sequence is analyzed in turn.

The same data set gives different branches of the global-and-local
procedure depending on the member-variable sequence analyzed and the
main variable.

``` r

analyze_bivariate <- function(label, fm_v1, sm_v1, fm_v2, sm_v2) {
  emp <- dyadicMarkov::countEmpBivariate(
    chainFM_V1 = fm_v1,
    chainSM_V1 = sm_v1,
    chainFM_V2 = fm_v2,
    chainSM_V2 = sm_v2,
    states = 2L
  )

  case <- dyadicMarkov::bivariateCase(emp, alpha = 0.05)

  cat("\n", label, "\n", sep = "")
  print(case)

  if (identical(case$case, "complete")) {
    print(dyadicMarkov::completePattern(emp))
  }

  if (identical(case$case, "partial")) {
    print(dyadicMarkov::partialPattern(emp))
  }

  if (identical(case$case, "univariate")) {
    print(dyadicMarkov::univariatePattern(fm_v1, sm_v1, states = 2L, alpha = 0.05))
  }
}

d <- dyadic_bivariate_example

analyze_bivariate(
  "FM_V1 as analyzed sequence, V1 as main variable",
  d$FM_V1, d$SM_V1, d$FM_V2, d$SM_V2
)
#> 
#> FM_V1 as analyzed sequence, V1 as main variable
#> Bivariate dyadic case
#> Case: complete
#> Alpha: 0.05
#> Dyadic interaction pattern
#> Pattern: complete actor on the main, actor partner on the second (D2)

analyze_bivariate(
  "SM_V1 as analyzed sequence, V1 as main variable",
  d$SM_V1, d$FM_V1, d$SM_V2, d$FM_V2
)
#> 
#> SM_V1 as analyzed sequence, V1 as main variable
#> Bivariate dyadic case
#> Case: complete
#> Alpha: 0.05
#> Dyadic interaction pattern
#> Pattern: complete actor partner on the main, partner on the second (D3)

analyze_bivariate(
  "FM_V2 as analyzed sequence, V2 as main variable",
  d$FM_V2, d$SM_V2, d$FM_V1, d$SM_V1
)
#> 
#> FM_V2 as analyzed sequence, V2 as main variable
#> Bivariate dyadic case
#> Case: partial
#> Alpha: 0.05
#> Dyadic interaction pattern
#> Pattern: partial actor (B2)

analyze_bivariate(
  "SM_V2 as analyzed sequence, V2 as main variable",
  d$SM_V2, d$FM_V2, d$SM_V1, d$FM_V1
)
#> 
#> SM_V2 as analyzed sequence, V2 as main variable
#> Bivariate dyadic case
#> Case: univariate
#> Alpha: 0.05
#> Dyadic interaction pattern
#> Pattern: APM (A1)
#> Alpha: 0.05
#> States: 2
```

For this example, the four role assignments illustrate three branches of
the procedure: complete bivariate cases for the two `V1` member-variable
sequences, a partial bivariate case for `FM_V2`, and a univariate case
for `SM_V2`. The partial branch is therefore demonstrated with the same
bivariate example data rather than with an artificial seeded example.

## Reading the global and local steps together

The global and local steps should be read together. If
[`bivariateCase()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/bivariateCase.md)
returns `trivial`, no subsequent local pattern is selected. If it
returns `univariate`, the bivariate workflow returns to
[`univariatePattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/univariatePattern.md)
using the first- and second-member sequences of the current main
variable. If it returns `partial`, the local step is
[`partialPattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/partialPattern.md).
If it returns `complete`, the local step is
[`completePattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/completePattern.md).

## References

Böllenrücher, Mégane, Joëlle Darwiche, and Jean-Philippe Antonietti. in
press. “Bivariate Dyadic Patterns Analysis Using Longitudinal
Actor-Partner Interdependence Model and Markov Chains for Single-Case.”
*Quantitative and Computational Methods in Behavioral Sciences*, ahead
of print, in press. <https://doi.org/10.23668/psycharchives.22174>.
