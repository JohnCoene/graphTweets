## Test environments
* local OS X install, R 3.5.1
* ubuntu 12.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

Received email below from CRAN team, moved `rtweet` to `Suggests` as recommended.

> This has been failing its checks for a long time and the maintainer has not responded to requests to fix.  It was scheduled for archival today. For
> 
> carbonate graphTweets needmining rehydratoR VOSONDash vosonSML
> 
> please move it to Suggests and use if conditonally (see §1.1.3.1 of 'Writing R Extensions')
> 
> and for bdpar use it conditionally.
> 
> ASAP (bearing in mind the CRAN shutdown from tomorrow until Jan 6) and before Jan 15 to safely retain the package on CRAN.
