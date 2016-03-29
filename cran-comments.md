## Test environments
* local OS X install, R 3.2.4
* ubuntu 12.04 (on travis-ci), R 3.2.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

* checking CRAN incoming feasibility ... NOTE (win-builder R-devel)
  Cannot figure out the cause

## FIX ##

* "your package does not work under R < 3.2.0 [...] Please fix or declare a proper version dependency on R"
  R dependency declared
* test failure
  tests skipped `testthat::skip_on_cran()`