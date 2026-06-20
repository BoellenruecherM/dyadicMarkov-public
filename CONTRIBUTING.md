# Contributing to dyadicMarkov

Thank you for your interest in contributing to `dyadicMarkov`.

`dyadicMarkov` is an R package for identifying interaction patterns in categorical dyadic sequences using transition matrices. Contributions are welcome, including bug reports, documentation improvements, examples, tests, and carefully discussed feature proposals.

## Reporting issues

Please report bugs, documentation issues, or feature requests through the GitHub issue tracker:

https://github.com/BoellenruecherM/dyadicMarkov-public/issues

When reporting a bug, please include:

* a minimal reproducible example;
* the expected result;
* the observed result;
* the version of `dyadicMarkov` you are using;
* your R session information, for example from `sessionInfo()`.

For questions about possible extensions or larger changes, please open an issue before starting substantial work. This helps avoid duplicate work and makes sure the proposed change fits the scope of the package.

## Fixing typos and documentation

Small documentation fixes, typo corrections, and improvements to examples are very welcome.

If you edit function documentation, please edit the source `.R` file containing the roxygen2 comments rather than the generated `.Rd` file. The `.Rd` files are generated from the roxygen2 documentation.

## Pull requests

Pull requests are welcome. For substantial changes, please open an issue first to discuss the proposed contribution.

A typical contribution workflow is:

1. Fork the repository.
2. Clone your fork locally.
3. Create a new branch for your change.
4. Make your changes.
5. Add or update tests when package behavior changes.
6. Update documentation when user-facing behavior changes.
7. Run the package checks locally.
8. Submit a pull request.

Before submitting a pull request, please make sure that:

* the package can be installed successfully;
* `devtools::test()` passes;
* `devtools::check()` passes without errors, warnings, or notes;
* new functionality is documented;
* new functionality is covered by tests where appropriate;
* user-facing changes are mentioned in `NEWS.md`.

## Code style

Please follow the general tidyverse style guide for R code. In particular:

* use clear and descriptive object names;
* use `snake_case` for function and variable names;
* avoid unnecessary changes to unrelated files;
* avoid restyling code that is not part of the pull request.

The package uses roxygen2 with Markdown syntax for documentation and testthat for unit tests.

Contributions that include clear documentation and relevant tests are easier to review and accept.

## Code of conduct

Please note that the `dyadicMarkov` project is released with a Contributor Code of Conduct. By contributing to this project, you agree to abide by its terms.
