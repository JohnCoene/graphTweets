library(graphTweets)

context("getNodes tests")

test_that("getNodes error", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"),
                       favorited = c(TRUE, FALSE),
                       stringsAsFactors = FALSE)
  edges <- tweets %>% 
    getEdges(text, screenName)
  
  expect_error(getNodes())
  
  # test invalid source and target
  expect_error(getNodes(tweets, error))
  expect_error(getNodes(tweets, source, error))
  
  lst <- list(tweets)
  expect_error(getNodes(lst))
})

test_that("getNodes tests", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"),
                       favorited = c(TRUE, FALSE),
                       stringsAsFactors = FALSE)
  edges <- getEdges(tweets, text, screenName)
  
  # nodes
  nodes <- getNodes(edges, source, target)
  
  # args
  e_edges <- getEdges(tweets, text, screenName, str.length = NULL, screenName, 
                      favorited)
  
  # class
  n_nodes <- getNodes(e_edges, source, target, favorited)
  
  # tests
  expect_equal(nrow(n_nodes), length(nodes))
  expect_is(nodes, "character")
  expect_is(n_nodes, "data.frame")
  
  # test length
  edg <- edges[1:4,]
  
  n <- getNodes(edg, source, target)
  
  expect_equal(nrow(n), 3)
})