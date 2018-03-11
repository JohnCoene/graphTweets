# graphTweets 0.4.0

Major release: overhaul to 1) make computations much faster, 2) make the whole package more tidyverse friendly and 3) switch to `rtweet` as main source.

* `getEdges` & `getNodes` are now deprecated in favour of `gt_edges` and `gt_nodes`
* `dynamise` deprecated in favour of `gt_dyn`
* `magrittr` pipe added.

# graphTweets 0.3.2

* documentation corrected
* Better extract @handles
* Better cleans @handles (i.e.: white space)

# graphTweets 0.3

* Fixed #1 issue, input check in `dynamise`
* Added `dynamise`
