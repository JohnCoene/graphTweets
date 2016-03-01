library(graphTweets)

context("test dynamise")

test_that("errors", {
  
  expect_error(dynamise())
  
  tweets <- get(load("tweets.RData"))
  
  # not data.frame
  lst <- as.list(tweets)
  expect_error(dynamise(lst))
  
  # wrong inputs
  expect_error(dynamise(tweets, tweets = "error"))
  expect_error(dynamise(tweets, tweets = "text", source = "error"))
  expect_error(dynamise(tweets, tweets = "text", source = "screenName", 
                        start.stamp = "error"))
})

test_that("test return", {
  
  tweets <- get(load("tweets.RData"))
  
  dyn <- dynamise(tweets, tweets = "text", source = "screenName", 
                  start.stamp = "created")
  
  expect_is(dyn, "igraph")
  
  expect_equal(length(dyn), 10)
  
  edges <- getEdges(data = tweets, "text", "screenName")
  
  edges$target <- as.character(edges$target)
  
  nodes <- getNodes(edges)
  
  g <- igraph::graph.data.frame(edges, directed = TRUE)
  
  expect_equal(length(igraph::V(g)), length(igraph::V(dyn)))
  
  expect_equal(length(igraph::V(dyn)), nrow(nodes))
  
  expect_equal(length(igraph::E(g)), length(igraph::E(dyn)))
  
  expect_equal(length(igraph::E(g)), nrow(edges))
  
})
