#' \pkg{graphTweets} visualise Twitter Interactions.
#' 
#' \itemize{
#' \item \code{\link{getEdges}} - get edges from tweets
#' \item \code{\link{getNodes}} - add meta-data to vertices
#' \item \code{\link{dynamise}} - create dynamic graphs
#' }
#' 
#' @examples 
#' \dontrun{
#' # authenticate
#' token <- twitteR::setup_twitter_oauth(consumer_key, consumer_secret, 
#'                                       access_token, access_secret)
#'                              
#' # search tweets
#' tweets <- twitteR::searchTwitter("rstats", n = 200)
#' 
#' # unlist to data.frame
#' tweets <- twitteR::twListToDF(tweets)
#' 
#' # load graphTweets
#' library(graphTweets)
#' 
#' # get edges
#' edges <- getEdges(data = tweets, tweets = "text", source = "screenName")
#' 
#' # load igraph
#' library(igraph)
#' 
#' # plot
#' g <- graph.data.frame(edges, directed=TRUE)
#' plot(g)
#' 
#' # add attributes to vertices
#' edges <- getEdges(data = tweets, tweets = "text", source = "screenName", 
#'                   "retweetCount")
#' nodes <- getNodes(edges, source = "source", target = "target", 
#'                   "retweetCount")
#'                   
#' g <- graph.data.frame(edges, directed=TRUE, vertices = nodes)
#' 
#' plot(g, vertex.size = V(g)$retweetCount)
#' 
#' # create dynamic graph and open in Gephi
#' dyn <- dynamise(tweets, tweets = "text", source = "screenName", 
#'                 start.stamp = "created", write = TRUE, open = TRUE)
#' }
#' 
#' @name graphTweets
#' @docType package
NULL