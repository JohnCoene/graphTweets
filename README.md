[![Build Status](https://travis-ci.org/JohnCoene/graphTweets.svg?branch=master)](https://travis-ci.org/JohnCoene/graphTweets)
[![Build status](https://ci.appveyor.com/api/projects/status/t37a595yg5eb2sx6/branch/master?svg=true)](https://ci.appveyor.com/project/JohnCoene/graphtweets/branch/master)
[![codecov.io](https://codecov.io/github/JohnCoene/graphTweets/coverage.svg?branch=master)](https://codecov.io/github/JohnCoene/graphTweets?branch=master)
[![Coverage Status](https://img.shields.io/coveralls/JohnCoene/graphTweets.svg)](https://coveralls.io/r/JohnCoene/graphTweets?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/graphTweets)](http://cran.r-project.org/package=graphTweets)
[![CRAN log](http://cranlogs.r-pkg.org/badges/grand-total/graphTweets)](http://cranlogs.r-pkg.org/badges/graphTweets)

# GraphTweets #

![gephi.gif](https://github.com/JohnCoene/docs/raw/master/output.gif)

Visualise networks of Twitter interactions.

* [Features](#features)
* [Install](#install)
* [Documentation](#documentaition)
* [Examples](#examples)

**In the process of updating the package to better suit `rtweets`**, see *`v4`* [install](#install) & [examples](#examples) (WIP--report issues) from ~~`v3.2`~~

## Features

~~`v3.2`~~

* ~~`getEdges`~~: build edge table from tweets.
* ~~`getNodes`~~: get nodes from edges.
* ~~`dynamise`~~: make a temporal graph.

*`v4`*

- `gt_edges` - get edges.
- `gt_nodes` - get nodes, with or without metadata.
- `gt_dyn` - create dynamic graph.
- `gt_graph` - create `igraph` graph object.
- `gt_save` - save the graph to file
- `gt_collect` - collect nodes and edges.

See `NEWS.md` for changes.

## Install

```R
install.packages("graphTweets") # CRAN release v3.2
devtools::install_github("JohnCoene/graphTweets") # dev version v4
```

## Documentation 

* [Examples](http://johncoene.github.io/projects/ex/graphTweets_examples.html)
* [Manual](http://johncoene.github.io/projects/docs/GraphTweets.pdf)

## Examples ##

*`v4`*

```R
library(rtweet)

# Sys.setlocale("LC_TIME", "English")

tweets <- search_tweets("#rstats")

library(graphTweets)

# simple network
tweets %>% 
  gt_edges(text, screen_name) %>% 
  gt_nodes %>% 
  gt_graph %>% 
  plot(.)

# dynamic graph
tweets %>% 
  gt_edges(text, screen_name, "created_at") %>% # add created time
  gt_nodes(TRUE) %>% 
  gt_graph %>% 
  gt_dyn %>% 
  gt_save # save as .graphml
```

~~`v3.2`~~

```R
# load twitteR to get tweets
library(twitteR)
token <- setup_twitter_oauth(consumer_key, consumer_secret, 
                             access_token=NULL, access_secret=NULL)
tweets <- searchTwitter("rstats", n = 200)
tweets <- twListToDF(tweets)

library(graphTweets)

edges <- getEdges(data = tweets, tweets = "text", source = "screenName")
nodes <- getNodes(edges)

# plot
g <- igraph::graph.data.frame(edges, directed=TRUE, vertices = nodes)

plot(g)
```