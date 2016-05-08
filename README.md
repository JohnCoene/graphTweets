[![Build Status](https://travis-ci.org/JohnCoene/graphTweets.svg?branch=master)](https://travis-ci.org/JohnCoene/graphTweets)
[![Build status](https://ci.appveyor.com/api/projects/status/t37a595yg5eb2sx6/branch/master?svg=true)](https://ci.appveyor.com/project/JohnCoene/graphtweets/branch/master)
[![codecov.io](https://codecov.io/github/JohnCoene/graphTweets/coverage.svg?branch=master)](https://codecov.io/github/JohnCoene/graphTweets?branch=master)
[![Coverage Status](https://img.shields.io/coveralls/JohnCoene/graphTweets.svg)](https://coveralls.io/r/JohnCoene/graphTweets?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/graphTweets)](http://cran.r-project.org/package=graphTweets)
[![CRAN log](http://cranlogs.r-pkg.org/badges/graphTweets)](http://cranlogs.r-pkg.org/badges/graphTweets)

![gephi.gif](https://github.com/JohnCoene/docs/raw/master/output.gif)

# GraphTweets #

Visualise networks of Twitter interactions

Features *three* functions:

* `getEdges`: build edge table from tweets
* `getNodes`: get nodes from edges
* `dynamise`: make a temporal graph

Build edge table and vertices to plot social network.

See `NEWS.md` for changes.

## Documentation ##

* [Examples](http://johncoene.github.io/projects/ex/graphTweets_examples.html)
* [Manual](http://johncoene.github.io/projects/docs/GraphTweets.pdf)

## Examples ##

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

## Other Projects ##

See my other projects at [http://johncoene.github.io/projects/](http://johncoene.github.io/projects/)