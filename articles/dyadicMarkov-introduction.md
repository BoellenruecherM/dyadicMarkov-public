# Introduction to dyadicMarkov

## Purpose of the package

`dyadicMarkov` implements an R workflow for identifying patterns of
interaction in categorical dyadic sequences using transition matrices.
The package is designed for situations in which one or two categorical
variables are observed repeatedly for both members of one dyad, so that
the analysis accounts for both temporal dependence and dyadic
dependence.

`dyadicMarkov` is based on three methodological papers on dyadic pattern
analysis with the Longitudinal Actor-Partner Interdependence Model
(L-APIM) and Markov chains. The univariate single-case method is
described by Bollenrücher et al. (2023). The extension to visualization
and clustering of similar dyadic behaviors is described by Bollenrücher
et al. (2024). The bivariate single-case method is described by
Böllenrücher et al. (in press). The visualization and clustering
methodology of Bollenrücher et al. (2024) is methodological background;
it is not currently part of the exported package API.

## Real-world use cases

The package is intended for ordered categorical observations collected
from two members of a dyad. In practice, such sequences may arise from
coded interaction data, daily diary studies, repeated binary responses,
or intensive longitudinal designs. For example, researchers may code
whether each partner shows a given behavior at each measurement
occasion, whether a parent and child are in one of several interaction
states, or whether two individuals report the presence or absence of a
response across repeated observations.

The package does not estimate a general-purpose time-series forecasting
model. Instead, it summarizes how the next state of the analyzed
sequence is associated with its own previous state and with the previous
state of the partner. The resulting pattern labels help describe whether
the observed transitions are better characterized by actor dependence,
partner dependence, actor-partner dependence, independence, or, in the
bivariate workflow, by partial or complete bivariate dependence
structures.

The example datasets included in the package are synthetic. They are
used to make the required input structure reproducible and easy to
inspect. They should be read as small stand-ins for real ordered dyadic
sequences, not as substantive empirical datasets.

## Data structure

The package works with categorical dyadic sequences. In the univariate
case, one categorical variable is observed over time for two members of
a dyad. For each function call, the first member is the member whose
next state is modeled, and the second member supplies the partner
sequence. The roles can be reversed to analyze the other member.

In the bivariate case, two categorical variables are observed over time
for both members of the dyad. The current implementation of the
bivariate workflow supports binary variables (`states = 2`). This leads
to a bivariate empirical count matrix with 16 rows and 2 columns: the 16
rows represent the four binary lagged components: the first member on
the main variable, the second member on the main variable, the first
member on the second variable and the second member on the second
variable. The 2 columns represent the possible next states of the first
member on the main variable.

The state-space scope differs between the two methods. The univariate
workflow supports any integer number of categorical \\states \ge 2\\.
The bivariate method is developed for two dichotomous variables and
supports `states = 2` only; bivariate analysis with more than two states
would require additional mathematical and software development.

## Estimation and identification

The package separates estimation from identification. Estimation
summarizes the observed sequences as empirical transition counts and
maximum-likelihood transition probabilities. Identification compares the
observed transition structure with restricted transition structures
corresponding to interpretable patterns of interaction.

In the univariate workflow, the relevant patterns are actor-partner,
actor-only, partner-only and independence. In the bivariate workflow,
the analysis first identifies the global case as trivial, univariate,
partial bivariate or complete bivariate. A trivial case has no
subsequent local pattern. A univariate case is followed by
[`univariatePattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/univariatePattern.md)
on the main-variable sequences. Partial and complete cases are followed
by
[`partialPattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/partialPattern.md)
and
[`completePattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/completePattern.md),
respectively.

The comparison statistics also differ by step. The univariate
pattern-identification procedure uses the Likelihood-Ratio Test (LRT)
nomenclature of the underlying method; `dyadicMarkov` evaluates these
comparisons using Pearson’s chi-squared statistic, \\X^2 = \sum (O -
E)^2 / E\\. The global bivariate approach compares nested models within
an LRT framework;
[`bivariateCase()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/bivariateCase.md)
implements two chi-squared tests for the A1 and B1 comparisons, also
evaluated using Pearson’s chi-squared statistic, \\X^2\\. The local
partial and complete bivariate procedures instead compute the G-squared
deviance, \\G^2 = 2 \sum O \log(O / E)\\, and then calculate \\AIC =
G^2 + 2k\\ for each candidate structure.

## Exported functions

The user-facing workflow is organized around seven exported functions:

- [`countEmp()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/countEmp.md)
  computes empirical transition counts for the first member sequence in
  a univariate dyadic sequence.
- [`mleEstimation()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/mleEstimation.md)
  estimates transition probabilities from empirical count matrices.
- [`univariatePattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/univariatePattern.md)
  identifies the univariate interaction pattern.
- [`countEmpBivariate()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/countEmpBivariate.md)
  computes empirical transition counts for the first member sequence in
  a bivariate dyadic sequence.
- [`bivariateCase()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/bivariateCase.md)
  identifies the global dependence case for the analyzed sequence.
- [`partialPattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/partialPattern.md)
  selects a local pattern for a partial bivariate case.
- [`completePattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/completePattern.md)
  selects a local pattern for a complete bivariate case.

## Assumptions and current scope

The workflow assumes categorical states coded as integers from 1 to
`states`, equal chain lengths, ordered repeated observations, and a
first-order homogeneous transition process. Inputs containing `NA` are
rejected; missing observations are not deleted or imputed automatically
because they break the construction of transition pairs.

Based on the sensitivity analysis reported in Böllenrücher et al. (in
press), a minimum sequence length of 90 measurement points is
recommended for applying the method. Pattern-identification accuracy
improves with longer sequences; for shorter sequences, particularly at
30 measurement points, the procedure often results in a trivial pattern,
whereas from 90 measurement points onward the occurrence of trivial
patterns diminishes significantly. This is methodological guidance
rather than a hard input requirement in `dyadicMarkov`.

The bivariate method and functions support the binary-state case. With
two binary variables observed for two members, the previous state is
described by four binary components, producing \\2^4 = 16\\
previous-state combinations. Generalizing this workflow beyond
`states = 2` would require additional mathematical development of the
bivariate restriction structures as well as software implementation.

## Relationship to the workflow vignettes

This introduction explains the scope and structure of the package. The
univariate workflow vignette shows the use of
[`countEmp()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/countEmp.md),
[`mleEstimation()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/mleEstimation.md)
and
[`univariatePattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/univariatePattern.md).
The bivariate workflow vignette shows the use of
[`countEmpBivariate()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/countEmpBivariate.md),
[`bivariateCase()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/bivariateCase.md),
[`partialPattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/partialPattern.md)
and
[`completePattern()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/completePattern.md).

## References

Bollenrücher, Mégane, Joëlle Darwiche, and Jean-Philippe Antonietti.
2023. “Dyadic Pattern Analysis Using Longitudinal Actor-Partner
Interdependence Model with Markov Chains for Unique Case Analysis.” *The
Quantitative Methods for Psychology* 19 (3): 230–43.
<https://doi.org/10.20982/tqmp.19.3.p230>.

Bollenrücher, Mégane, Joëlle Darwiche, and Jean-Philippe Antonietti.
2024. “Methodology for Identification, Visualization, and Clustering of
Similar Behaviors in Dyadic Sequences Analyzed Through the Longitudinal
Actor-Partner Interdependence Model with Markov Chains.” *The
Quantitative Methods for Psychology* 20 (1): 17–32.
<https://doi.org/10.20982/tqmp.20.1.p017>.

Böllenrücher, Mégane, Joëlle Darwiche, and Jean-Philippe Antonietti. in
press. “Bivariate Dyadic Patterns Analysis Using Longitudinal
Actor-Partner Interdependence Model and Markov Chains for Single-Case.”
*Quantitative and Computational Methods in Behavioral Sciences*, ahead
of print, in press. <https://doi.org/10.23668/psycharchives.22174>.
