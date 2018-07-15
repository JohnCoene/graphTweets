## Test environments
* local OS X install, R 3.4.4
* ubuntu 12.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

Removed `splitstackshape` dependency, see email below.

> Dear maintainers,
> 
> This concerns the CRAN packages
>
>  Diderot graphTweets rodham sampler splitstackshape

> maintained by one of you:
>
>  Ananda Mahto <ananda@mahto.info>: splitstackshape
>  Christian Vincenot <christian@vincenot.biz>: Diderot
>  John Coene <jcoenep@gmail.com>: graphTweets rodham
>  Michael Baldassaro <mbaldassaro@gmail.com>: sampler
>
> We have repeatedly asked for an update fixing the check problems
> shown on
>   <https://cran.r-project.org/web/checks/check_results_splitstackshape.html>
> with no reply from the maintainer thus far.
> 
> Thus, package splitstackshape is now scheduled for archival on
> 2018-07-29, and archiving this will necessitate also archiving its
> strong reverse dependencies.
> 
> Please negotiate the necessary actions.

