## Overview

`dyadicMarkov` provides methods for analyzing categorical dyadic sequences using transition matrices within the Longitudinal Actor-Partner Interdependence Model (L-APIM) and Markov-chain framework. The package supports empirical transition counts, maximum likelihood estimation of transition probabilities, and identification of univariate and bivariate patterns of interaction in dyadic sequences.

The package is designed for single-case dyadic sequence analysis, where two members of a dyad are observed repeatedly over time. It allows researchers to describe the temporal dynamics of each member while accounting for the possible influence of their partner.

## Installation

You can install the released version of `dyadicMarkov` from CRAN:

    install.packages("dyadicMarkov")

Then load the package with:

    library(dyadicMarkov)

The official CRAN page is available here:

<https://cran.r-project.org/package=dyadicMarkov>

## Methodological background

The methodological foundation of `dyadicMarkov` is based on research on dyadic sequences, the Longitudinal Actor-Partner Interdependence Model, and Markov chains.

The 2023 article introduces the use of Markov chains for modeling categorical longitudinal dyadic data in a single-case perspective. The 2024 article extends the approach to the identification, visualization, and clustering of similar behaviors in samples of dyads. The bivariate extension is documented in related work by Bollenrücher, Antonietti, and Darwiche.

### Related papers

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (2023).
*Dyadic pattern analysis using longitudinal Actor-Partner Interdependence Model with Markov chains for unique case analysis*.
The Quantitative Methods for Psychology, 19(3), 230-245.
DOI: [10.20982/tqmp.19.3.p230](https://doi.org/10.20982/tqmp.19.3.p230)

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (2024).
*Methodology for identification, visualization, and clustering of similar behaviors in dyadic sequences analyzed through the longitudinal Actor-Partner Interdependence Model with Markov chains*.
The Quantitative Methods for Psychology, 20(1), 17-32.
DOI: [10.20982/tqmp.20.1.p017](https://doi.org/10.20982/tqmp.20.1.p017)

Bollenrücher, M., Antonietti, J.-P., & Darwiche, J. (2026).
*Bivariate dyadic patterns in categorical sequences: A Markovian approach within the Longitudinal Actor-Partner Interdependence Model*.
DOI: [10.23668/psycharchives.22174](https://doi.org/10.23668/psycharchives.22174)
