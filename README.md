# dyadicMarkov

<!-- cranlogs:start -->

<table>
  <tr>
    <td style="vertical-align: middle; padding-right: 14px;">Pattern Identification<br>for Dyadic Sequences<br>Using Transition Matrices</td>
    <td>
      <a href="https://CRAN.R-project.org/package=dyadicMarkov"><img src="https://www.r-pkg.org/badges/version/dyadicMarkov" alt="CRAN version"></a><br>
      <a href="https://cran.r-project.org/web/checks/check_results_dyadicMarkov.html"><img src="https://badges.cranchecks.info/worst/dyadicMarkov.svg" alt="CRAN checks"></a><br>
      <a href="https://boellenruecherm.r-universe.dev/dyadicMarkov"><img src="https://boellenruecherm.r-universe.dev/dyadicMarkov/badges/version" alt="R-universe version"></a><br>
      <a href="https://boellenruecherm.r-universe.dev/dyadicMarkov"><img src="https://boellenruecherm.r-universe.dev/dyadicMarkov/badges/checks" alt="R-universe checks"></a><br>
      <a href="https://www.r-pkg.org/pkg/dyadicMarkov"><img src="https://cranlogs.r-pkg.org/badges/last-month/dyadicMarkov?color=blue" alt="CRAN downloads last month"></a><br>
      <a href="https://www.r-pkg.org/pkg/dyadicMarkov"><img src="https://cranlogs.r-pkg.org/badges/grand-total/dyadicMarkov?color=blue" alt="CRAN downloads total"></a><br>
      <a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg" alt="Project Status: Active"></a><br>
<a href="https://github.com/BoellenruecherM/dyadicMarkov-public/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/BoellenruecherM/dyadicMarkov-public/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check"></a>
    </td>
  </tr>
</table>

<sub>Download counts are recorded from the RStudio/Posit CRAN mirror via <code>cranlogs</code>.</sub>

<!-- Last automatic update: 2026-05-24 -->

<!-- cranlogs:end -->

## Overview

`dyadicMarkov` provides methods for analyzing dyadic interaction sequences using transition matrices within the Actor-Partner Interdependence Model. The package supports the computation of empirical transition counts, maximum likelihood estimation of transition probabilities and identification of interaction patterns in univariate and bivariate dyadic interaction sequences.

The package is designed for categorical dyadic sequences, where two members of a dyad are observed repeatedly over time. It allows researchers to model the temporal dynamics of each member while accounting for the possible influence of their partner.

## Installation

You can install the released version of `dyadicMarkov` from CRAN:

```r
install.packages("dyadicMarkov")
```

Then load the package with:

```r
library(dyadicMarkov)
```

The official CRAN page is available here:  
[CRAN page for dyadicMarkov](https://cran.r-project.org/package=dyadicMarkov)

The CRAN page includes the package reference manual, vignette, source files and additional package metadata.

## Methodological background

The methodological foundation of `dyadicMarkov` is based on research on dyadic sequences, longitudinal Actor-Partner Interdependence Models and Markov chains.

The first article introduces the use of Markov chains for modeling categorical longitudinal dyadic data in a single-case perspective. The second article extends the approach to the identification, visualization and clustering of similar behaviors in samples of dyads.

### Related papers

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (2023).  
*Dyadic pattern analysis using longitudinal Actor-Partner Interdependence Model with Markov chains for unique case analysis*.  
The Quantitative Methods for Psychology, 19(3), 230–245.  
DOI: [10.20982/tqmp.19.3.p230](https://doi.org/10.20982/tqmp.19.3.p230)  
PDF: [PDF](https://www.tqmp.org/RegularArticles/vol19-3/p230/p230.pdf)

Bollenrücher, M., Darwiche, J., & Antonietti, J.-P. (2024).  
*Methodology for identification, visualization, and clustering of similar behaviors in dyadic sequences analyzed through the longitudinal Actor-Partner Interdependence Model with Markov chains*.  
The Quantitative Methods for Psychology, 20(1), 17–32.  
DOI: [10.20982/tqmp.20.1.p017](https://doi.org/10.20982/tqmp.20.1.p017)  
PDF: [PDF](https://www.tqmp.org/RegularArticles/vol20-1/p017/p017.pdf)

A package paper will be added here once available.

## Citation

To cite the package in publications, please use:

```r
citation("dyadicMarkov")
```

## License

This package is released under the license specified in the `LICENSE` file.
