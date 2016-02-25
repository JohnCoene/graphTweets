library(graphTweets)

context("getEdges tests")

test_that("test getEdges errors", {
  
  tweets <- get(load("test_dat.RData"))
  
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

test_that("test str.Length", {
  
  tweets <- get(load("test_dat.RData"))
  
  edges5 <- getEdges(tweets, "text", "screenName", str.length = 5)
  
  for(i in 1:nrow(edges5)) {
    
    expect_true(length(edges5$source[i]) <= 5)
    expect_true(length(edges5$target[i]) <= 5)
  }
  
})