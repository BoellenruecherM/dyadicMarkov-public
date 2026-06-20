# dyadicMarkov: Pattern Identification for Dyadic Sequences Using Transition Matrices

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

## Main terminology

A dyadic sequence records the categorical states of two interacting
individuals over time. Empirical transition counts summarize transitions
from previous dyadic states to subsequent states of the focal member.
Transition probabilities are estimated by normalizing each row of the
empirical transition count matrix. Patterns of interaction are
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
related to the current state of the focal member.

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

- <https://boellenruecherm.github.io/dyadicMarkov-public/>

- <https://github.com/BoellenruecherM/dyadicMarkov-public>

- Report bugs at
  <https://github.com/BoellenruecherM/dyadicMarkov-public/issues>

## Author

**Maintainer**: Mattia Böllenrücher
<mattia.boellenruecher@student.unisg.ch> \[copyright holder\]

Authors:

- Mégane Bollenrücher

- Jean-Philippe Antonietti
