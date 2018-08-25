## Test environments
* local OS X install, R 3.4.4
* ubuntu 12.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

Bug fixes and an additional function.

NEWS.md:

> # graphTweets 0.5.0
> 
> * `gt_co_edges` replaces `gt_edges_hashes`, it now works with other columns than just `hashtags`.
> * `gt_edges_hash` is _deprecated_ in favour of `gt_edges`.
> * `gt_edges_bind` and `gt_co_edges_bind` added to bind edges together and build more complex graphs.
> * Escape hatches, functions ending in `_` are no longer necessary and are thus deprecated.

