[![Build Status](https://travis-ci.org/JohnCoene/graphTweets.svg?branch=master)](https://travis-ci.org/JohnCoene/graphTweets)
[![codecov.io](https://codecov.io/github/JohnCoene/graphTweets/coverage.svg?branch=master)](https://codecov.io/github/JohnCoene/graphTweets?branch=master)

# GraphTweets #

Visualise networks of Twitter interactions

Features only *one* functions:

* `getEdges`: build edge table from tweets
* `getNodes`: get nodes from edges

Build edge table and vertices to plot social network.

## Examples ##

```
# load twitteR
library(twitteR)

# authenticate
token <- setup_twitter_oauth(consumer_key, consumer_secret, 
                             access_token=NULL, access_secret=NULL)

# search tweets
tweets <- searchTwitter("rstats", n = 200)

# unlist to data.frame
tweets <- twListToDF(tweets)

# get edges
edges <- getEdges(data = tweets, tweets = "text", source = "screenName")

# get nodes
nodes <- getNodes(edges)

# load igraph
library(igraph)

# plot
g <- graph.data.frame(edges, directed=TRUE, vertices = nodes)

plot(g)
```