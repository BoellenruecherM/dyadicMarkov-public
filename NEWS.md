# dyadicMarkov 0.1.2 (2026-08-21)

* Clarified that the univariate pattern-identification procedure is an LRT
  procedure evaluated using Pearson's chi-squared statistic, while the global bivariate
  nested-model/LRT framework implements two chi-squared tests for A1 and B1,
  also evaluated using Pearson's chi-squared statistic.
* Clarified that local bivariate pattern selection computes the G-squared
  deviance before applying `AIC = G^2 + 2k`.
* Corrected the univariate pattern-identification and global bivariate case
  boundaries so that p-values equal to alpha are treated as rejection
  (`p <= alpha`).
* Documented that the univariate workflow supports multiple categorical states,
  while the bivariate workflow is defined for two dichotomous variables.
* Added focused tests that distinguish Pearson's chi-squared statistic from
  G-squared and verify both partial and complete bivariate AIC paths.
* Updated the maintainer email address and package version for this release.
* Made the manual simulated-parity script stop when a comparison fails.
* Declared `srr` as a development/documentation dependency.

# dyadicMarkov 0.1.1 (2026-06-21)

* Updated package wording and metadata for the CRAN submission.
* Added S3 classes and print/summary support for pattern and case identification results.
* Added S3 classes for empirical count matrices and MLE transition probability matrices while preserving
  ordinary matrix behavior.
* Added two synthetic 90-point example datasets for package workflow examples.
* Rewrote the workflow vignette around the built-in univariate and bivariate example datasets.
* Improved internal input validation for count, estimation, and pattern-identification functions.
* Updated tests and documentation for the new S3 return objects.
* Improved validation for extreme state-space inputs, non-finite chain values, and malformed empirical
  matrices.
* Refactored selected internal validation and AIC helper code to reduce function complexity while preserving exported
  behavior.
* Improved bivariate count validation coverage for unsupported and malformed inputs.

# dyadicMarkov 0.1.0 (2026-03-16)

* Initial CRAN submission.
