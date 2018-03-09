library(rtweet)

twitter_token <- readRDS("twitter_token.rds")

tweets <- search_tweets("#rstats")
