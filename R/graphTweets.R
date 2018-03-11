#' \pkg{graphTweets} visualise Twitter Interactions.
#'  
#' @section v4:
#' \itemize{
#' \item \code{\link{gt_edges}} \code{\link{gt_edges_}} - get edges from tweets
#' \item \code{\link{gt_nodes}} - add meta-data to vertices
#' \item \code{\link{gt_dyn}} - create dynamic graphs
#' \item \code{\link{gt_collect}} - collect edges and nodes
#' \item \code{\link{gt_graph}} - create graph
#' \item \code{\link{gt_save}} - save graph to file
#' }
#' 
#'  
#' @section v3.2:
#' \itemize{
#' \item \code{\link{getEdges}} - get edges from tweets
#' \item \code{\link{getNodes}} - add meta-data to vertices
#' \item \code{\link{dynamise}} - create dynamic graphs
#' }
#' 
#' @keywords internal
#' 
#' @examples 
#' \dontrun{
#' ## v4
#' library(rtweet)
#' 
#' # Sys.setlocale("LC_TIME", "English")
#' 
#' tweets <- search_tweets("#rstats")
#' 
#' library(graphTweets)
#' 
#' # simple network
#' tweets %>% 
#'   gt_edges(text, screen_name) %>% # get edges
#'   gt_nodes %>% # get nodes
#'   gt_graph %>% # build igraph object
#'   plot(.)
#'   
#' # dynamic graph
#' tweets %>% 
#'   gt_edges(text, screen_name, "created_at") %>% # add created time
#'   gt_nodes(TRUE) %>% 
#'   gt_graph %>% 
#'   gt_dyn %>% # make dynamic
#'   gt_save # save as .graphml
#' 
#' ## v3.2
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