# GraphTweets

[![Build Status](https://travis-ci.org/JohnCoene/graphTweets.svg?branch=master)](https://travis-ci.org/JohnCoene/graphTweets)
[![Build status](https://ci.appveyor.com/api/projects/status/t37a595yg5eb2sx6/branch/master?svg=true)](https://ci.appveyor.com/project/JohnCoene/graphtweets/branch/master)
[![codecov.io](https://codecov.io/github/JohnCoene/graphTweets/coverage.svg?branch=master)](https://codecov.io/github/JohnCoene/graphTweets?branch=master)
[![Coverage Status](https://coveralls.io/repos/github/JohnCoene/graphTweets/badge.svg?branch=master)](https://coveralls.io/github/JohnCoene/graphTweets?branch=master)
![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/graphTweets)
[![CRAN log](http://cranlogs.r-pkg.org/badges/grand-total/graphTweets)](http://cranlogs.r-pkg.org/badges/graphTweets)
[![twinetverse](https://img.shields.io/badge/twinetverse-0.0.2-yellow.svg)](http://twinetverse.john-coene.com/)

![](https://raw.githubusercontent.com/JohnCoene/projects/master/img/graphTweets.png)

Visualise networks of Twitter interactions.

* [Install](#install)
* [Documentation](#documentation)
* [Features](#features)
* [Rationale](#rationale)
* [Examples](#examples)

## Install

```R
install.packages("graphTweets") # CRAN release v0.4
devtools::install_github("JohnCoene/graphTweets") # dev version
```

## Documentation 

* [Examples](http://graphtweets.john-coene.com/)

## Functions

- `gt_edges` - get edges.
- `gt_nodes` - get nodes, with or without metadata.
- `gt_dyn` - create dynamic graph.
- `gt_graph` - create `igraph` graph object.
- `gt_save` - save the graph to file
- `gt_collect` - collect nodes and edges.
