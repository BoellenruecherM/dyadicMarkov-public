# dyadicMarkov

## Overview

`dyadicMarkov` provides methods for analyzing categorical dyadic sequences using transition matrices within the Longitudinal Actor-Partner Interdependence Model (L&#8209;APIM) and Markov-chain framework. The package supports empirical transition counts, maximum likelihood estimation of transition probabilities, and identification of univariate and bivariate patterns of interaction in dyadic sequences.

The package is designed for settings in which two members of a dyad are observed repeatedly over time. Examples include daily diary data, coded interaction sequences, repeated binary responses, and intensive longitudinal designs in which researchers want to describe how each member's next state relates to their own previous state and to their partner's previous state.

The core question addressed by the package is whether the observed transition structure is best described by an actor-partner, actor-only, partner-only, independence, partial bivariate, or complete bivariate pattern, depending on the workflow used.

## When to use this package

Use `dyadicMarkov` when:

* two members of one dyad are observed repeatedly over ordered measurement occasions;
* the observed states are categorical and coded as integers;
* the analysis focuses on transition structures rather than continuous outcomes;
* the research question concerns temporal dependence, dyadic dependence, or both;
* the goal is to estimate transition probabilities and identify interpretable interaction patterns.

The univariate workflow analyzes one categorical variable observed for two dyad members. The bivariate workflow analyzes two categorical variables observed for both members. The current bivariate implementation supports binary variables (`states = 2`). With two binary variables observed for two members, the previous state is described by four binary components, producing a bivariate empirical count matrix with 16 rows and 2 columns.

## Installation

You can install the released version of `dyadicMarkov` from CRAN:

    install.packages("dyadicMarkov")

You can install the development version from R-universe:

    install.packages(
      "dyadicMarkov",
      repos = c("https://boellenruecherm.r-universe.dev", "https://cloud.r-project.org")
    )

Alternatively, you can install the development version from GitHub:

    # install.packages("pak")
    pak::pak("BoellenruecherM/dyadicMarkov-public")

Then load the package with:

    library(dyadicMarkov)

## Example data

The package includes small synthetic datasets that illustrate the required input structure and the package workflow. They are not intended to represent a substantive empirical study. They are used to show how ordered categorical dyadic sequences are transformed into transition count matrices and then into pattern-identification results.

For complete examples, see the workflow vignettes:

* [Introduction to dyadicMarkov](articles/dyadicMarkov-introduction.html)
* [Univariate dyadic workflow](articles/univariate-workflow.html)
* [Bivariate dyadic workflow](articles/bivariate-workflow.html)

## Methodological background

The methodological foundation of `dyadicMarkov` is based on research on categorical dyadic sequences, the Longitudinal Actor-Partner Interdependence Model, and Markov chains.

The 2023 article introduces the use of Markov chains for modeling categorical longitudinal dyadic data in a single-case perspective. The 2024 article extends the approach to the identification, visualization, and clustering of similar behaviors in samples of dyads. The bivariate extension is documented in related work by Bollenrücher, Darwiche, and Antonietti.

The visualization and clustering methodology is part of the methodological background of the project. It is not currently part of the exported package API.

### Related papers

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (2023).
*Dyadic pattern analysis using longitudinal Actor-Partner Interdependence Model with Markov chains for unique case analysis*.
The Quantitative Methods for Psychology, 19(3), 230-243.
DOI: [10.20982/tqmp.19.3.p230](https://doi.org/10.20982/tqmp.19.3.p230)

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (2024).
*Methodology for identification, visualization, and clustering of similar behaviors in dyadic sequences analyzed through the longitudinal Actor-Partner Interdependence Model with Markov chains*.
The Quantitative Methods for Psychology, 20(1), 17-32.
DOI: [10.20982/tqmp.20.1.p017](https://doi.org/10.20982/tqmp.20.1.p017)

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (in press).
*Bivariate dyadic patterns analysis using longitudinal Actor-Partner Interdependence Model and Markov chains for single-case*.
Quantitative and Computational Methods in Behavioral Sciences.
DOI: [10.23668/psycharchives.22174](https://doi.org/10.23668/psycharchives.22174)

---

Please note that this project follows the [rOpenSci Code of Conduct](https://ropensci.org/code-of-conduct/). By contributing to this project, you agree to abide by its terms.
