# dyadicMarkov

<!-- cranlogs:start -->

<table>
  <tr>
    <td style="vertical-align: middle; padding-right: 14px;">Pattern Estimation and Identification<br>for Dyadic Sequences<br>Using Transition Matrices in R</td>
    <td>
      <a href="https://CRAN.R-project.org/package=dyadicMarkov"><img src="https://www.r-pkg.org/badges/version-ago/dyadicMarkov" alt="CRAN version and release age"></a><br>
      <a href="https://cran.r-project.org/web/checks/check_results_dyadicMarkov.html"><img src="https://badges.cranchecks.info/worst/dyadicMarkov.svg" alt="CRAN checks"></a><br>
      <a href="https://boellenruecherm.r-universe.dev/dyadicMarkov"><img src="https://boellenruecherm.r-universe.dev/dyadicMarkov/badges/version" alt="R-universe version"></a><br>
      <a href="https://boellenruecherm.r-universe.dev/dyadicMarkov"><img src="https://boellenruecherm.r-universe.dev/dyadicMarkov/badges/checks" alt="R-universe checks"></a><br>
      <a href="https://www.r-pkg.org/pkg/dyadicMarkov"><img src="https://cranlogs.r-pkg.org/badges/last-month/dyadicMarkov?color=blue" alt="CRAN downloads last month"></a><br>
      <a href="https://www.r-pkg.org/pkg/dyadicMarkov"><img src="https://cranlogs.r-pkg.org/badges/grand-total/dyadicMarkov?color=blue" alt="CRAN downloads total"></a><br>
      <a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg" alt="Project Status: Active"></a><br>
      <a href="https://github.com/BoellenruecherM/dyadicMarkov-public/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/BoellenruecherM/dyadicMarkov-public/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main" alt="R-CMD-check"></a>
    </td>
  </tr>
</table>

<sub>Download counts are recorded from the RStudio/Posit CRAN mirror via <code>cranlogs</code>.</sub>

<!-- Last automatic update: 2026-05-25 -->

<!-- cranlogs:end -->

## Overview

`dyadicMarkov` provides methods for analyzing categorical dyadic sequences using transition matrices within the Longitudinal Actor-Partner Interdependence Model (L-APIM) and Markov-chain framework. The package supports empirical transition counts, maximum likelihood estimation of transition probabilities, and identification of univariate and bivariate patterns of interaction in dyadic sequences.

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

```r
install.packages("dyadicMarkov")
```

You can install the development version from R-universe:

```r
install.packages(
  "dyadicMarkov",
  repos = c("https://boellenruecherm.r-universe.dev", "https://cloud.r-project.org")
)
```

Alternatively, you can install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("BoellenruecherM/dyadicMarkov-public")
```

Then load the package with:

```r
library(dyadicMarkov)
```

The official CRAN page is available here:
[CRAN page for dyadicMarkov](https://cran.r-project.org/package=dyadicMarkov)

## Basic example

The package includes small synthetic datasets that illustrate the required input structure and the package workflow. They are not intended to represent a substantive empirical study. They are used to show how ordered categorical dyadic sequences are transformed into transition count matrices and then into pattern-identification results.

```r
library(dyadicMarkov)

data("dyadic_univariate_example", package = "dyadicMarkov")

head(dyadic_univariate_example)

emp <- countEmp(
  chainFM = dyadic_univariate_example$FM,
  chainSM = dyadic_univariate_example$SM,
  states = 2
)

fit <- mleEstimation(emp)

pattern <- univariatePattern(
  chainFM = dyadic_univariate_example$FM,
  chainSM = dyadic_univariate_example$SM,
  states = 2,
  alpha = 0.05
)

emp
fit
pattern
summary(pattern)
```

In this example, `chainFM` is the member sequence analyzed and `chainSM` is the partner sequence. Reversing the two arguments analyzes the dyad from the perspective of the other member.

## Methodological background

The methodological foundation of `dyadicMarkov` is based on research on categorical dyadic sequences, the Longitudinal Actor-Partner Interdependence Model, and Markov chains.

The 2023 article introduces the use of Markov chains for modeling categorical longitudinal dyadic data in a single-case perspective. The 2024 article extends the approach to the identification, visualization, and clustering of similar behaviors in samples of dyads. The bivariate extension is documented in related work by Bollenrücher, Darwiche, and Antonietti.

The visualization and clustering methodology is part of the methodological background of the project. It is not currently part of the exported package API.

### Related papers

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (2023).
*Dyadic pattern analysis using longitudinal Actor-Partner Interdependence Model with Markov chains for unique case analysis*.
The Quantitative Methods for Psychology, 19(3), 230–243.
DOI: [10.20982/tqmp.19.3.p230](https://doi.org/10.20982/tqmp.19.3.p230)
PDF: [PDF](https://www.tqmp.org/RegularArticles/vol19-3/p230/p230.pdf)

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (2024).
*Methodology for identification, visualization, and clustering of similar behaviors in dyadic sequences analyzed through the longitudinal Actor-Partner Interdependence Model with Markov chains*.
The Quantitative Methods for Psychology, 20(1), 17–32.
DOI: [10.20982/tqmp.20.1.p017](https://doi.org/10.20982/tqmp.20.1.p017)
PDF: [PDF](https://www.tqmp.org/RegularArticles/vol20-1/p017/p017.pdf)

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (in press).
*Bivariate dyadic patterns analysis using longitudinal Actor-Partner Interdependence Model and Markov chains for single-case*.
Quantitative and Computational Methods in Behavioral Sciences.
DOI: [10.23668/psycharchives.22174](https://doi.org/10.23668/psycharchives.22174)

## Citation

To cite the package in publications, please use:

```r
citation("dyadicMarkov")
```

## License

This package is released under the license specified in the `LICENSE` file.
