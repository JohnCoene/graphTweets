library(graphTweets)

context("test dynamise")

test_that("errors", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"),
                       start = c(1, 2),
                       end = c(3, 4),
                       stringsAsFactors = FALSE)
  
  expect_error(dynamise())
  
  # not data.frame
  lst <- as.list(tweets)
  expect_error(dynamise(lst))
  
  # wrong columns names
  expect_error(dynamise(tweets, tweets = "error"))
  expect_error(dynamise(tweets, tweets = "text", source = "error"))
  expect_error(dynamise(tweets, tweets = "text", source = "screenName", 
                        start.stamp = "error"))
  expect_error(dynamise(tweets, tweets = "text", source = "screenName", 
                        start.stamp = "start", end.stamp = "error"))
  
  # end.stamp and start.stamp different classes
  expect_error(dynamise(tweets, tweets = "text", source = "screenName", 
                        start.stamp = "start", end.stamp = "text"))
  
  tweets$start <- as.character(tweets$start)
  
  expect_error(dynamise(tweets, tweets = "text", source = "screenName", 
                        start.stamp = "start"))
  
  expect_warning(dynamise(tweets, tweets = "text", source = "screenName", 
                        start.stamp = "end", write = FALSE, open = TRUE))
})

test_that("test return", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"),
                       start = c(1, 2),
                       end = c(3, 4),
                       stringsAsFactors = FALSE)
  
  dyn <- dynamise(tweets, tweets = "text", source = "screenName", 
                  start.stamp = "start")
  
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
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"),
                       start = c(1, 2),
                       end = c(3, 4),
                       stringsAsFactors = FALSE)  
  
  dyn1 <- dynamise(tweets, tweets = "text", source = "screenName", 
                  start.stamp = "start", end.stamp = 2)
  
  tweets$end <- tweets$start + 1
  
  dyn2 <- dynamise(tweets, tweets = "text", source = "screenName", 
                  start.stamp = "start", end.stamp = "end")
  
  tweets$date <- as.Date(c("2016-01-01", "2016-02-02"))
  
  dyn3 <- dynamise(tweets, tweets = "text", source = "screenName", 
                  start.stamp = "date", end.stamp = 1)
  
  expect_equal(length(igraph::V(dyn1)), length(igraph::V(dyn2)))
  expect_equal(length(igraph::V(dyn2)), length(igraph::V(dyn3)))
  expect_equal(length(igraph::V(dyn3)), length(igraph::V(dyn1)))
  
  expect_equal(length(igraph::E(dyn1)), length(igraph::E(dyn2)))
  expect_equal(length(igraph::E(dyn2)), length(igraph::E(dyn3)))
  expect_equal(length(igraph::E(dyn3)), length(igraph::E(dyn1)))
})
