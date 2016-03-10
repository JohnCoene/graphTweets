library(graphTweets)

context("test dynamise")

test_that("errors", {
  
  expect_error(dynamise())
  
  tweets <- get(load("tweets.RData"))
  
  # not data.frame
  lst <- as.list(tweets)
  expect_error(dynamise(lst))
  
  # wrong columns names
  expect_error(dynamise(tweets, tweets = "error"))
  expect_error(dynamise(tweets, tweets = "text", source = "error"))
  expect_error(dynamise(tweets, tweets = "text", source = "screenName", 
                        start.stamp = "error"))
  expect_error(dynamise(tweets, tweets = "text", source = "screenName", 
                        start.stamp = "created", end.stamp = "error"))
  
  # end.stamp and start.stamp different classes
  expect_error(dynamise(tweets, tweets = "text", source = "screenName", 
                        start.stamp = "created", end.stamp = "retweetCount"))
  
  tweets$start <- as.character(tweets$created)
  
  expect_error(dynamise(tweets, tweets = "text", source = "screenName", 
                        start.stamp = "start"))
  
  expect_warning(dynamise(tweets, tweets = "text", source = "screenName", 
                        start.stamp = "created", write = FALSE, open = TRUE))
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

test_that("test end.stamp", {
  
  tweets <- get(load("tweets.RData"))
  
  dyn1 <- dynamise(tweets, tweets = "text", source = "screenName", 
                  start.stamp = "created", end.stamp = 3600)
  
  tweets$end <- tweets$created + 7200
  
  dyn2 <- dynamise(tweets, tweets = "text", source = "screenName", 
                  start.stamp = "created", end.stamp = "end")
  
  tweets$date <- as.Date(tweets$created)
  
  dyn3 <- dynamise(tweets, tweets = "text", source = "screenName", 
                  start.stamp = "date", end.stamp = 1)
  
  expect_equal(length(igraph::V(dyn1)), length(igraph::V(dyn2)))
  expect_equal(length(igraph::V(dyn2)), length(igraph::V(dyn3)))
  expect_equal(length(igraph::V(dyn3)), length(igraph::V(dyn1)))
  
  expect_equal(length(igraph::E(dyn1)), length(igraph::E(dyn2)))
  expect_equal(length(igraph::E(dyn2)), length(igraph::E(dyn3)))
  expect_equal(length(igraph::E(dyn3)), length(igraph::E(dyn1)))
})
