## Test environments
* local OS X install, R 3.4.4
* ubuntu 12.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

Bug fixes and an additional function.

NEWS.md:

> # graphTweets 0.4.3
>
> * Added `gt_edges_hashes_` and `gt_edges_hashes` to build networks of #hashtags co-mentions.
> * Added `%<-%` from the zeallot package to unpack the nodes and edges.
> * Fixed `gt_dyn` bug where lifetime was not working properly.

