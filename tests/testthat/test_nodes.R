library(graphTweets)

context("getNodes tests")

test_that("getNodes error", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"),
                       favorited = c(TRUE, FALSE),
                       stringsAsFactors = FALSE)
  edges <- getEdges(tweets, "text", "screenName")
  
  expect_error(getNodes())
  
  # test invalid source and target
  expect_error(getNodes(tweets, source = "error"))
  expect_error(getNodes(tweets, source = "source", target = "error"))
  
  lst <- list(tweets)
  expect_error(getNodes(lst))
})

test_that("getNodes tests", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"),
                       favorited = c(TRUE, FALSE),
                       stringsAsFactors = FALSE)
  edges <- getEdges(tweets, "text", "screenName")
  
  # class
  edges$source <- as.character(edges$source)
  edges$target <- as.character(edges$target)
  
  # nodes
  nodes <- getNodes(edges)
  
  # args
  e_edges <- getEdges(tweets, "text", str.length = NULL, "screenName", 
                      "favorited")
  
  # class
  e_edges$source <- as.character(e_edges$source)
  e_edges$target <- as.character(e_edges$target)  
  n_nodes <- getNodes(e_edges, source = "source", target = "target", 
                      "favorited")
  
  # tests
  expect_equal(nrow(n_nodes), nrow(nodes))
  expect_is(nodes, "data.frame")
  expect_is(n_nodes, "data.frame")
  
  # test length
  edg <- edges[1:5,]
  
  n <- getNodes(edg)
  
  expect_equal(nrow(n), 4)
})