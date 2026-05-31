## Test environments

Local checks:

- Windows 11 x64, R 4.5.2

GitHub Actions:

- macOS latest, R-release
- Windows latest, R-release
- Ubuntu latest, R-devel
- Ubuntu latest, R-release
- Ubuntu latest, R-oldrel-1

Win-Builder:

- R-release: Status OK
- R-oldrelease: Status OK
- R-devel: Status OK

## R CMD check results

Local `devtools::check(clean = TRUE, manual = TRUE, args = "--as-cran")`:

- 0 errors
- 0 warnings
- 0 notes

Local `devtools::check_built(..., args = "--as-cran")` on the built tarball:

- 0 errors
- 0 warnings
- 0 notes

Win-Builder:

- R-release: Status OK
- R-oldrelease: Status OK
- R-devel: Status OK

GitHub Actions:

- All configured jobs passed.

## rOpenSci statistical software standards

The package documents compliance with the applicable rOpenSci statistical software standards using `srr`.

- `srr::srr_stats_pre_submit(path = ".", quiet = FALSE)` passed.
- 36 standards are documented as complied with.
- 32 standards are documented as not applicable.

## Downstream dependencies

There are no downstream dependencies for this package.
