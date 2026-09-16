# dyadicMarkov 0.1.3.9000

* Refined argument documentation for rOpenSci review, including explicit state-space constraints, separate documentation of the four bivariate sequence inputs, and clarification that bivariate empirical count matrices may use integer or double storage modes.

# dyadicMarkov 0.1.3

* Added a sensitivity-analysis vignette reproducing the published
  sequence-length analysis for 1,000 simulated dyads at five sequence lengths,
  with shorter worked examples for 30- and 90-point sequences.
* Added the fixed simulation datasets used in the sensitivity analysis and
  documented their structure, binary state space, historical labels, and
  separate simulation at each sequence length.
* Clarified the terminology used in the sensitivity analysis and noted the
  small numerical differences from the published sensitivity and specificity
  results.
* Documented how exact AIC ties are handled in partial and complete bivariate
  pattern selection.
* Improved validation of the `digits` argument used by print, summary, and
  LaTeX methods.
* Added a check for state-space sizes that are too large to construct the
  required transition-count matrices safely.
* Updated the statistical-software-review annotations to reflect the legacy
  implementation parity tests.
* Added and updated tests for the changes above.
* Added base-R state-strip `plot()` methods for univariate pattern results and
  binary bivariate case and pattern results, with accessible defaults and
  retained sequence metadata.
* Added inferential summary tables and LaTeX representations for empirical-count
  and MLE matrices.

# dyadicMarkov 0.1.2

* Clarified that the univariate pattern-identification procedure is an LRT
  procedure evaluated using Pearson Chi-squared, while the global bivariate
  nested-model/LRT framework implements two chi-squared tests for A1 and B1,
  also evaluated using Pearson Chi-squared.
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

# dyadicMarkov 0.1.1

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

# dyadicMarkov 0.1.0

* Initial CRAN submission.
