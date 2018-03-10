library(rtweet)

twitter_token <- readRDS("twitter_token.rds")

Sys.setlocale("LC_TIME", "English")

tweets <- search_tweets("#rstats", token = twitter_token)
