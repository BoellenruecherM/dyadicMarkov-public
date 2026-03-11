## Test environments
* local Windows 11 x64, R 4.5.2
* GitHub Actions:
  - macOS-latest (release)
  - windows-latest (release)
  - ubuntu-latest (devel)
  - ubuntu-latest (release)
  - ubuntu-latest (oldrel-1)
* win-builder (R-release)
* win-builder (R-devel)

## R CMD check results
* Local `devtools::check(clean = TRUE, manual = TRUE, args = "--as-cran")`:
  - 0 errors
  - 0 warnings
  - 0 notes
* Local `devtools::check_built(..., args = "--as-cran")` on the built tarball:
  - 0 errors
  - 0 warnings
  - 0 notes

## Resubmission
* This is a new submission.

## Notes
* Win-Builder returned 1 NOTE on both R-release and R-devel:
  - "New submission"
