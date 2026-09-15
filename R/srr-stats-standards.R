#' rOpenSci statistical software standards
#'
#' This file records compliance annotations for the rOpenSci statistical
#' software review standards that apply to dyadicMarkov. Standards that are not
#' applicable are listed separately with brief justifications.
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.4} All exported functions are documented using roxygen2, and devtools::document() generates the corresponding Rd files.
#' @srrstats {G1.4a} All internal helper functions are documented with roxygen2 blocks and @noRd tags, including validation helpers, likelihood-ratio comparison helpers, bivariate chi-squared helpers, G-squared/AIC helpers, and S3 print helper functions.
#' @srrstats {G1.5} The sensitivity-analysis vignette reproduces the sequence-length performance analysis using the simulation datasets included with dyadicMarkov. It provides the complete classification code for 1,000 simulated dyads at sequence lengths 30, 60, 90, 180, and 720 and reports the resulting sensitivity and specificity values.
#' @srrstats {G3.0} Exact numeric equality is used only for categorical state values validated to be integer-valued, integer validation, observed count totals, and structural-zero checks in transition-count matrices. dyadicMarkov does not compare floating-point statistical estimates for exact equality.
#' @srrstats {G5.1} The package created example data sets are exported as package data and are used in shipped tests to verify their structure, state coding, and expected workflow classifications.
#' @srrstats {G5.3} Shipped tests check that returned empirical counts and MLE probability matrices contain finite, non negative values and valid probabilities after row normalization.
#' @srrstats {G5.4} Correctness tests use fixed hand constructed dyadic sequences and empirical count matrices with known outputs for transition counting, estimation, Pearson chi-squared comparisons, and G-squared/AIC calculations.
#' @srrstats {G5.4a} Because the dyadic transition matrix method is package specific, implementation correctness is tested against hand computed cases, including exact counts, zero row MLE behavior, deterministic Pearson statistics that differ from G-squared, and G-squared plus parameter-penalty AIC values.
#' @srrstats {G5.4b} The current implementation has been compared with the archived legacy implementation using fixed parity tests maintained in the source repository.
#' @srrstats {G5.6} Parameter recovery is tested for mleEstimation() using empirical transition count matrices with known probabilities obtained by row normalization.
#' @srrstats {G5.6a} Parameter recovery tests compare recovered transition probabilities with known expected probabilities using an explicit numerical tolerance.
#' @srrstats {G5.7} Scaling behaviour is tested in two ways: mleEstimation() is invariant to proportional scaling of empirical transition counts, and deterministic regression tests using bundled sensitivity data verify the expected A1, B2, D4, and E4 classifications across fixed simulation fixtures at different sequence lengths. The complete 1,000-dyad sequence-length analysis is reproduced in the sensitivity-analysis vignette.
#' @srrstats {G5.9} Noise susceptibility is tested where meaningful for dyadicMarkov: trivial representable numerical perturbations of empirical transition count matrices are tested for stability of mleEstimation(), while seed and initial-condition sensitivity are not applicable because the estimation and pattern-identification algorithms are deterministic.
#' @srrstats {G5.9a} Numerical-stability tests perturb empirical transition count matrices by trivial representable noise and verify that mleEstimation() results do not meaningfully change within numerical tolerance.
#' @srrstats {G5.10} The full five-length sensitivity reproduction is included in the testthat framework and is enabled with DYADICMARKOV_EXTENDED_TESTS=true; ordinary test runs retain only the lightweight sequence-length regression test.
#' @srrstats {G5.12} CONTRIBUTING.md documents how to enable the extended sensitivity test, its bundled-data requirements, deterministic behaviour, measured runtime, platform considerations, memory requirements, and absence of generated artefacts requiring manual inspection.
#' @srrstats {EA5.0} Sequence plots use labelled member and variable rows, explicit categorical state legends, and consistent state colours across all displayed sequences.
#' @srrstats {EA5.0a} Plot text uses readable base-graphics defaults and can be scaled through the validated cex argument.
#' @srrstats {EA5.0b} Binary state strips use accessible blue and orange defaults that differ in both hue and luminance; higher-state univariate plots use a qualitative HCL palette, and custom colours are strictly validated.
#' @srrstats {EA5.4} Sequence-plot ticks are generated with pretty() while explicitly retaining the first and final measurement occasions and excluding occasion zero.
#' @srrstats {EA5.5} Sequence plots label the horizontal measurement index as t, while categorical state identities are explicitly shown in the legend.
#' @srrstats {EA6.0d} Raw and summary inferential tables are tested for their documented schemas, dimensions, column types, source-faithful candidate ordering, and preservation of the underlying statistical results.
#' @srrstats {EA6.1} Tests verify strip run geometry, state-colour mappings, sequence metadata propagation, plot specifications, graphical-parameter restoration, informative plotting errors, and successful PDF-device output for univariate and bivariate results.
#' @noRd
NULL


#' NA_standards
#'
#' The following standards are recorded as not applicable to the current package
#' scope. Each annotation gives the reason for non applicability.
#'
#' @srrstatsNA {G1.6} dyadicMarkov does not currently make comparative performance claims against alternative R implementations.
#' @srrstatsNA {G2.3} dyadicMarkov does not use free form univariate character parameters to control statistical algorithms.
#' @srrstatsNA {G2.3a} Not applicable because dyadicMarkov does not use free form univariate character parameters requiring match.arg().
#' @srrstatsNA {G2.3b} Not applicable because dyadicMarkov does not use case sensitive free form character parameters.
#' @srrstatsNA {G2.4b} dyadicMarkov does not require conversion of inputs to continuous numeric values. Categorical states are represented by numeric vectors with integer-valued entries, empirical transition counts are checked as numeric matrices, and continuous valued statistical inputs are not part of the package API.
#' @srrstatsNA {G2.4c} dyadicMarkov does not require conversion of inputs to character for its statistical algorithms.
#' @srrstatsNA {G2.4d} dyadicMarkov does not require conversion of inputs to factor for its statistical algorithms.
#' @srrstatsNA {G2.4e} dyadicMarkov does not require conversion from factor inputs for its statistical algorithms.
#' @srrstatsNA {G2.5} dyadicMarkov does not expect factor inputs; categorical states are represented through numeric state vectors with integer-valued entries.
#' @srrstatsNA {G2.7} dyadicMarkov operates on state vectors and transition count matrices, not on general tabular time series containers.
#' @srrstatsNA {G2.9} dyadicMarkov does not perform lossy type conversions or metadata altering conversions such as factor to character or spatial metadata removal.
#' @srrstatsNA {G2.10} dyadicMarkov does not extract single columns from tabular inputs as part of its statistical algorithms.
#' @srrstatsNA {G2.11} dyadicMarkov does not accept data.frame like tabular inputs with non standard column classes as primary statistical inputs.
#' @srrstatsNA {G2.12} dyadicMarkov does not accept data.frame like tabular inputs with list columns as primary statistical inputs.
#' @srrstatsNA {G2.14b} dyadicMarkov does not ignore missing data because doing so would alter empirical transition counts.
#' @srrstatsNA {G2.14c} dyadicMarkov does not impute missing categorical states because imputation would alter empirical transition counts and inferred transition patterns.
#' @srrstatsNA {G3.1} dyadicMarkov does not rely on covariance calculations.
#' @srrstatsNA {G3.1a} Not applicable because dyadicMarkov does not use covariance calculations.
#' @srrstatsNA {G4.0} dyadicMarkov does not write statistical outputs to local files.
#' @srrstatsNA {G5.0} Not applicable because no external standard reference data set exists for the package specific dyadic categorical transition matrix methods implemented in dyadicMarkov; correctness is instead tested using hand constructed data with known properties.
#' @srrstatsNA {G5.5} dyadicMarkov's correctness tests and core statistical algorithms are deterministic and do not generate random values, so a fixed random seed is not required.
#' @srrstatsNA {G5.6b} dyadicMarkov's core algorithms are deterministic and do not involve random seeds or random initial conditions.
#' @srrstatsNA {G5.9b} dyadicMarkov's estimation and pattern-identification routines are deterministic and do not depend on random seeds or iterative initial conditions.
#' @srrstatsNA {G5.11} dyadicMarkov does not require large external data sets or external assets for extended tests.
#' @srrstatsNA {G5.11a} Not applicable because dyadicMarkov does not download external data for tests.
#' @srrstatsNA {EA2.0} dyadicMarkov does not accept standard tabular data requiring extensive filtering or joins, so an index column system is not applicable.
#' @srrstatsNA {EA2.1} Not applicable because dyadicMarkov does not use index columns for table filtering or joining operations.
#' @srrstatsNA {EA2.2} Not applicable because dyadicMarkov does not use index columns or table join workflows.
#' @srrstatsNA {EA2.2a} Not applicable because no index-column class system is used or required.
#' @srrstatsNA {EA2.2b} Not applicable because no index-column attribute is used or required.
#' @srrstatsNA {EA2.3} dyadicMarkov does not perform table join operations.
#' @srrstatsNA {EA2.4} dyadicMarkov does not accept multi-tabular input.
#' @srrstatsNA {EA2.5} Not applicable because dyadicMarkov does not accept multi-tabular input or use index columns.
#' @srrstatsNA {EA5.1} Plot methods inherit the graphics-device typeface and do not override the font family.
#' @srrstatsNA {EA5.6} Not applicable because dyadicMarkov does not bundle dynamic visualization libraries.
#' @noRd
NULL
