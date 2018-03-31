[![Build Status](https://travis-ci.org/JohnCoene/graphTweets.svg?branch=master)](https://travis-ci.org/JohnCoene/graphTweets)
[![Build status](https://ci.appveyor.com/api/projects/status/t37a595yg5eb2sx6/branch/master?svg=true)](https://ci.appveyor.com/project/JohnCoene/graphtweets/branch/master)
[![codecov.io](https://codecov.io/github/JohnCoene/graphTweets/coverage.svg?branch=master)](https://codecov.io/github/JohnCoene/graphTweets?branch=master)
[![Coverage Status](https://coveralls.io/repos/github/JohnCoene/graphTweets/badge.svg?branch=master)](https://coveralls.io/github/JohnCoene/graphTweets?branch=master)
![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/graphTweets)
[![CRAN log](http://cranlogs.r-pkg.org/badges/grand-total/graphTweets)](http://cranlogs.r-pkg.org/badges/graphTweets)

# GraphTweets #

![gephi.gif](http://john-coene.com/img/graphTweets.png)

Visualise networks of Twitter interactions.

* [Install](#install)
* [Documentation](#documentation)
* [Features](#features)
* [Rationale](#rationale)
* [Examples](#examples)

**Updated the package to better suit `rtweets`**

## Install

```R
install.packages("graphTweets") # CRAN release v0.4
devtools::install_github("JohnCoene/graphTweets") # dev version
```

## Documentation 

* [Examples](http://john-coene.com/packages/graphTweets/)

## Features

*`v4`*

- `gt_edges` - get edges.
- `gt_nodes` - get nodes, with or without metadata.
- `gt_dyn` - create dynamic graph.
- `gt_graph` - create `igraph` graph object.
- `gt_save` - save the graph to file
- `gt_collect` - collect nodes and edges.

See `NEWS.md` for changes.

## Rationale

Functions are meant to be run in a specific order.

1. Extract edges
2. Extract the nodes

One can only know the nodes of a network based on the edges, so run them in that order. However, you can build a graph based on edges alone:

```R
tweets %>% 
  gt_edges(text, screen_name) %>% 
  gt_graph() %>% 
  plot(., vertex.size = igraph::degree(.) * 10)
```

This is useful if you are building a large graph and don't need any meta data on the nodes (other than those you can compute from the graph, i.e.: `degree` like in the example above). If you need meta data on the nodes use `gt_nodes`.

```R
tweets %>% 
  gt_edges(text, screen_name) %>% 
  gt_nodes(meta = TRUE) %>% # set meta to TRUE
  gt_graph() %>% 
  plot(., vertex.size = v(.)$followers_count) # size nodes by follower count.
```

## Examples

*`v4`*

```R
library(rtweet)

# Sys.setlocale("LC_TIME", "English")

tweets <- search_tweets("#rstats")

library(graphTweets)

# simple network
tweets %>% 
  gt_edges(text, screen_name) %>% # get edges
  gt_nodes %>% # get nodes
  gt_graph %>% # build igraph object
  plot(.)

# dynamic graph
tweets %>% 
  gt_edges(text, screen_name, "created_at") %>% # add created time
  gt_nodes(TRUE) %>%
  gt_dyn %>% # make dynamic
  gt_save # save as .graphml
```
