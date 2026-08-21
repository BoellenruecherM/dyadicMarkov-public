## Test environments

Local checks:

* Windows 11 x64, R 4.6.1

Remote Windows checks using win-builder:

* R-oldrelease 4.5.3
* R-release 4.6.1
* R-devel (2026-08-17 r90424)

## R CMD check results

Final source tarball:

* File: dyadicMarkov_0.1.2.tar.gz
* SHA256: D5A87A50C6273973F1D6019FC85F3C18857080AB7D59E568AF33BE5238B8CB35
* Size: 59,111 bytes

Local R CMD check --as-cran on the final source tarball:

* 0 errors
* 0 warnings
* 1 note

The single NOTE reports the intentional maintainer email change from
mattia.boellenruecher@student.unisg.ch to
mboellenruec@student.ethz.ch.

Windows win-builder checks on the same final source tarball:

* R-oldrelease 4.5.3: 0 errors, 0 warnings, 1 note
* R-release 4.6.1: 0 errors, 0 warnings, 1 note
* R-devel (2026-08-17 r90424): 0 errors, 0 warnings, 1 note

In all three win-builder checks, the single NOTE is the same intentional
maintainer email change.

## rOpenSci statistical software standards

The package documents compliance with the applicable rOpenSci statistical
software standards using srr.

Final pre-submission check:

* srr::srr_stats_pre_submit() passed.
* 53 standards are documented as complied with.
* 49 standards are documented as not applicable.
