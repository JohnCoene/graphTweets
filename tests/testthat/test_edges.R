library(graphTweets)

context("getEdges tests")

test_that("test getEdges errors", {
  
  skip_on_cran()
  
  # setup
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"), 
                       stringsAsFactors = FALSE)
  
  # test required
  expect_error(getEdges(tweets))
  
  # test missing source or text
  expect_error(getEdges(tweets, tweets = "text"))
  expect_error(getEdges(tweets, source = "screenName"))
  
  # test str.Length
  expect_error(getEdges(tweets, "text", "screenName", str.length = "error"))
  
  # test classes
  txt <- tweets
  txt$text <- as.factor(paste(txt$text))
  expect_error(getEdges(txt, "text", "screenName"))
  
  src <- tweets
  src$screenName <- as.factor(paste(src$screenName))
  expect_error(getEdges(src, "text", "screenName"))
  
  # invalid columns
  expect_error(getEdges(tweets, tweets = "error", source = "screenName"))
  expect_error(getEdges(tweets, tweets = "text", source = "error"))
  
  # test object class
  tweets <- list(tweets)
  expect_error(getEdges(tweets))
})

test_that("test getEdges no additional arguments", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"), 
                       stringsAsFactors = FALSE)
  
  # names
  edges <- getEdges(tweets, "text", "screenName")
  expect_equal(names(edges), c("source", "target"))
  
  edges <- getEdges(tweets, "text", "screenName")
  
  expect_equal(nrow(edges), 4)
})

test_that("test getEdges additional arguments", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"),
                       favorited = c(TRUE, FALSE),
                       stringsAsFactors = FALSE)
  
  # names
  edges <- getEdges(data = tweets, tweets = "text", source = "screenName",
                    str.length = NULL, "favorited")
  expect_equal(names(edges), c("source", "target", "favorited"))
  
  expect_equal(nrow(edges), 4)
  
  edges <- getEdges(data = tweets, tweets = "text", source = "screenName",
                    str.length = NULL, "favorited")
  expect_equal(names(edges), c("source", "target", "favorited"))
  
  expect_equal(nrow(edges), 4)
})

test_that("test str.Length", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"),
                       favorited = c(TRUE, FALSE),
                       stringsAsFactors = FALSE)
  
  edges5 <- getEdges(tweets, "text", "screenName", str.length = 2)
  
  for(i in 1:nrow(edges5)) {
    
    expect_true(length(edges5$source[i]) <= 3)
    expect_true(length(edges5$target[i]) <= 3)
  }
  
})