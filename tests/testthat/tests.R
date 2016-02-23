library(graphTweets)
library(testthat)

test_that("test getEdges errors", {
  tweets <- get(load("test_dat.RData"))
  
  # test required
  expect_error(edges(tweets))
  
  # test classes
  tweets$text <- as.factor(paste(tweets$text))
  expect_error(edges(tweets, "text", "screenName"))
  tweets$screenName <- as.factor(paste(tweets$screenName))
  expect_error(edges(tweets, "text", "screenName"))
  
  # test object class
  tweets <- list(tweets)
  expect_error(edges(data, "text", "screenName"))
})

test_that("test getEdges no additional arguments", {
  tweets <- get(load("test_dat.RData"))
  
  # names
  edges <- getEdges(tweets, "text", "screenName")
  expect_equal(names(edges), c("source", "target"))
  
  # nrow
  samp <- tweets[1:10,]
  edges <- getEdges(samp, "text", "screenName")
  
  expect_equal(nrow(edges), 5)
})

test_that("test getEdges additional arguments", {
  tweets <- get(load("test_dat.RData"))
  
  # names
  edges <- getEdges(data = tweets, tweets = "text", source = "screenName",
                    str.length = NULL, "longitude", "latitude")
  expect_equal(names(edges), c("source", "target", "longitude", "latitude"))
  
  expect_equal(nrow(edges), 238)
  
  edges <- getEdges(data = tweets, tweets = "text", source = "screenName",
                    str.length = NULL, "text", "favorited")
  expect_equal(names(edges), c("source", "target", "text", "favorited"))
  
  expect_equal(nrow(edges), 238)
})