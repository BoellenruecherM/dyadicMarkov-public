## Test environments

Local checks:

* Windows 11 x64, R 4.5.2

Local final tarball check:

* Windows 11 x64, R 4.5.2, R CMD check --no-manual --as-cran dyadicMarkov_0.1.1.tar.gz: Status OK

Win-Builder:

* R-release, R 4.6.0: Status OK
* R-oldrelease, R 4.5.3: Status OK
* R-devel, R Under development: Status OK

R-hub / GitHub Actions:

* Linux, R-devel: Status OK
* Windows, R-devel: Status OK

## R CMD check results

Local devtools::check using --as-cran and compact vignettes:

* 0 errors
* 0 warnings
* 0 notes

Local R CMD check --no-manual --as-cran on the final built tarball:

* Status OK

Win-Builder:

* R-release: Status OK
* R-oldrelease: Status OK
* R-devel: Status OK

R-hub / GitHub Actions:

* Linux, R-devel: Status OK
* Windows, R-devel: Status OK

## rOpenSci statistical software standards

The package documents compliance with the applicable rOpenSci statistical software standards using srr.

* srr::srr_stats_pre_submit() passed.
* 53 standards are documented as complied with.
* 49 standards are documented as not applicable.

## Downstream dependencies

There are no downstream dependencies for this package.
