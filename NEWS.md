# graphTweets 0.4.1

* `gt_nodes` returns number of `n_edges`, the number of edges the node is present in.
* `gt_edges_hash` and respective escape hatch added! 

# graphTweets 0.4.0

Major release: overhaul to 1) make computations much faster, 2) make the whole package more tidyverse friendly and 3) switch to `rtweet` as main source.

* `getEdges` & `getNodes` are now deprecated in favour of `gt_edges` and `gt_nodes`
* `dynamise` deprecated in favour of `gt_dyn`
* `magrittr` pipe added.
* `gt_collect` added: use to get to collect edges and nodes as list.
* `gt_graph` added: use to convert to igraph object.

Performance

```r
library(graphTweets)
library(rtweet)

token <- create_token("APP", "xxxXXxxxx", "xXXXxxXX")
tweets <- search_tweets("#rstats", token = token)

rbenchmark::benchmark(
  "v3.2" = {
    edges <- getEdges(as.data.frame(tweets), "screen_name", "text")
    g <- igraph::graph.data.frame(edges, TRUE)
  },
  "v4" = {
    tweets %>% 
      gt_edges_() %>%  
      gt_graph() -> g
  }
)

  test replications elapsed relative user.self sys.self user.child sys.child
1 v3.2          100    6.55    1.492      6.45     0.06         NA        NA
2   v4          100    4.39    1.000      4.33     0.05         NA        NA
```

# graphTweets 0.3.2

* documentation corrected
* Better extract @handles
* Better cleans @handles (i.e.: white space)

# graphTweets 0.3

* Fixed #1 issue, input check in `dynamise`
* Added `dynamise`
