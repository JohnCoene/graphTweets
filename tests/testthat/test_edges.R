library(graphTweets)

context("getEdges tests")

test_that("test getEdges errors", {
  
  # setup
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you",
                                "I tweet @him."),
                       screenName = c("me", "him", "me"),
                       retweetCount = c(4, 2, 1),
                       favorited = c(TRUE, FALSE, NA),
                       stringsAsFactors = FALSE)
  
  # test required
  expect_error(getEdges(tweets))
  
  # test missing source or text
  expect_error(getEdges(tweets, text))
  expect_error(getEdges(tweets, screenName))
  
  # test str.Length
  expect_error(getEdges(tweets, text, screenName, error))
  
  # test classes
  txt <- tweets
  txt$text <- as.factor(paste(txt$text))
  
  # invalid columns
  expect_error(getEdges(tweets, error, screenName))
  expect_error(getEdges(tweets, text, error))
  
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
  edges <- getEdges(tweets, text, screenName)
  expect_equal(names(edges), c("source", "target"))
  
  edges <- getEdges(tweets, text, screenName)
  
  expect_equal(nrow(edges), 4)
})

test_that("test getEdges additional arguments", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"),
                       favorited = c(TRUE, FALSE),
                       stringsAsFactors = FALSE)
  
  # names
  edges <- getEdges(data = tweets, text, screenName,
                    str.length = NULL, favorited)
  expect_equal(names(edges), c("source", "target", "favorited"))
  
  expect_equal(nrow(edges), 4)
  
  edges <- getEdges(data = tweets, text, screenName,
                    str.length = NULL, favorited)
  expect_equal(names(edges), c("source", "target", "favorited"))
  
  expect_equal(nrow(edges), 4)
})

test_that("test str.Length", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screenName = c("me", "him"),
                       favorited = c(TRUE, FALSE),
                       stringsAsFactors = FALSE)
  
  edges5 <- getEdges(tweets, text, screenName, str.length = 2)
  
  for(i in 1:nrow(edges5)) {
    
    expect_true(length(edges5$source[i]) <= 3)
    expect_true(length(edges5$target[i]) <= 3)
  }
  
})