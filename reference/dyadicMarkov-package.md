# dyadicMarkov: Pattern Estimation and Identification for Dyadic Sequences Using Transition Matrices in R

The dyadicMarkov package provides tools for analyzing categorical dyadic
sequences using transition matrices in the Longitudinal Actor-Partner
Interdependence Model and Markov-chain framework. It supports empirical
transition counts, maximum likelihood estimation of transition
probabilities, and identification of univariate and bivariate patterns
of interaction.

## Statistical scope

dyadicMarkov is designed for single-case categorical dyadic sequences.
The temporal structure is represented by the order of the observations.
The main outputs are empirical count matrices, estimated transition
probability matrices, and identified patterns of interaction.

## Supported state spaces

The univariate workflow supports any integer number of categorical
states \\S \ge 2\\. The bivariate method is developed for two
dichotomous variables and therefore supports `states = 2` only,
producing 16-by-2 empirical count matrices. Bivariate support beyond two
states would require additional mathematical and software development
and is not implemented by this package.

## Comparison statistics

The univariate pattern-identification procedure is a likelihood-ratio
test (LRT) procedure that dyadicMarkov evaluates using Pearson's
chi-squared statistic. The global bivariate approach compares nested
models within an LRT framework;
[`bivariateCase()`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/bivariateCase.md)
implements two chi-squared tests for the A1 and B1 comparisons,
evaluated using Pearson's chi-squared statistic. Local partial and
complete bivariate pattern selection is separate: it computes the
G-squared deviance and then \\AIC = G^2 + 2k\\ for each candidate
structure.

## Main terminology

A dyadic sequence records the categorical states of two interacting
individuals over time. Empirical transition counts summarize transitions
from previous dyadic states to subsequent states of the analyzed
sequence. Transition probabilities are estimated by normalizing each row
of the empirical transition count matrix. Patterns of interaction are
identified by comparing unrestricted and restricted transition
structures based on actor and partner effects.

## Lifecycle statement

dyadicMarkov is under active development. The current version focuses on
univariate and bivariate categorical dyadic sequences. The core exported
functions are intended to remain stable across minor releases.

## Method

The method models categorical dyadic sequences with Markov chains in the
Longitudinal Actor-Partner Interdependence Model (L-APIM) framework. It
uses transition matrices to represent how previous dyadic states are
related to the current state of the analyzed sequence.

## Algorithmic contribution

The package implements the main computational steps of the method:
empirical transition counts, estimation of transition probabilities, and
identification of patterns of interaction in univariate and bivariate
cases.

## References

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (2023). Dyadic
pattern analysis using longitudinal Actor-Partner Interdependence Model
with Markov chains for unique case analysis. *The Quantitative Methods
for Psychology*, 19(3), 230–243.
[doi:10.20982/tqmp.19.3.p230](https://doi.org/10.20982/tqmp.19.3.p230)

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (2024). Methodology
for identification, visualization, and clustering of similar behaviors
in dyadic sequences analyzed through the longitudinal Actor-Partner
Interdependence Model with Markov chains. *The Quantitative Methods for
Psychology*, 20(1), 17–32.
[doi:10.20982/tqmp.20.1.p017](https://doi.org/10.20982/tqmp.20.1.p017)

Böllenrücher, M., Darwiche, J., & Antonietti, J.-P. (in press).
Bivariate dyadic patterns analysis using longitudinal actor-partner
interdependence model and Markov chains for single-case. *Quantitative
and Computational Methods in Behavioral Sciences*.
[doi:10.23668/psycharchives.22174](https://doi.org/10.23668/psycharchives.22174)

Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). *Dyadic Data
Analysis*. Guilford Press.

Bakeman, R., & Quera, V. (2011). *Sequential Analysis and Observational
Methods for the Behavioral Sciences*. Cambridge University Press.

## See also

Useful links:

- <https://github.com/BoellenruecherM/dyadicMarkov-public>

- <https://boellenruecherm.github.io/dyadicMarkov-public/>

- Report bugs at
  <https://github.com/BoellenruecherM/dyadicMarkov-public/issues>

## Author

**Maintainer**: Mattia Böllenrücher <mboellenruec@student.ethz.ch>
([ORCID](https://orcid.org/0009-0004-4149-4745)) \[copyright holder\]

Authors:

- Mattia Böllenrücher <mboellenruec@student.ethz.ch>
  ([ORCID](https://orcid.org/0009-0004-4149-4745)) \[copyright holder\]

- Mégane Bollenrücher ([ORCID](https://orcid.org/0000-0002-9035-8799))

- Jean-Philippe Antonietti
  ([ORCID](https://orcid.org/0000-0003-0117-4769))
