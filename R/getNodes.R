#' getNodes
#' 
#' @description get nodes from a data.frame of edges as typically returned by 
#' \code{\link{getEdges}}
#' 
#' @param edges data.frame of edges as typically returned by 
#' \code{\link{getEdges}}
#' @param source Column of source nodes in \code{edges}, must be a 
#' \code{character} string, defaults to \code{source}.
#' @param target Column of target nodes in \code{edges}, must be a 
#' \code{character} string, required.
#' @param ... Any other columns to be passed on to the \code{source} nodes - 
#' will not be applied to \code{target} nodes.
#' 
#' @details Duplicate values are dropped, additional arguments (\code{...}) are 
#' only applied to nodes from \code{source}.
#' 
#' @examples 
#' \dontrun{
#' # load twitteR
#' library(twitteR)
#' 
#' # authenticate
#' token <- setup_twitter_oauth(consumer_key, consumer_secret, 
#'                              access_token=NULL, access_secret=NULL)
#'                              
#' # search tweets
#' tweets <- searchTwitter("rstats", n = 200)
#' 
#' # unlist to data.frame
#' tweets <- twListToDF(tweets)
#' 
#' # get edges
#' edges <- getEdges(data = tweets, tweets = "text", source = "screenName")
#' 
#' # get nodes
#' nodes <- getNodes(edges)
#' 
#' # load igraph
#' library(igraph)
#' 
#' # plot
#' g <- graph.data.frame(edges, directed=TRUE, vertices = nodes)
#' 
#' plot(g)
#' }
#' 
#' @author John Coene \email{jcoenep@@gmail.com}
#' 
#' @export
getNodes <- function(edges, source = "source", target = "target", ...) {
  
  # check inputs
  if (class(edges) != "data.frame") {
    stop("edges must be a data.frame")
  } 
  
  if (class(edges[, source]) != "character"){
    stop("source must be of class character")
  }
  
  if (class(edges[, target]) != "character"){
    stop("target must be of class character")
  } 
  
  args <- unlist(list(...))
  
  if(length(args)) {
    
    # split
    src <- unique(edges[, c(source, args)])
    tgt <- data.frame(target = as.character(unique(edges[, target])))
    
    # remove duplicates
    src <- src[!duplicated(src[, source]),]
    
    # to character
    src[, source] <- as.character(src[, source])
    tgt[, target] <- as.character(tgt[, target])
    
    # anti_join
    tgt <- dplyr::anti_join(tgt, src, by = c(target = source))
    
    # adds ards
    tgt[, args] <- NA
    
    # rename for bind
    names(tgt)[1] <- source
    
    # bind
    nodes <- rbind.data.frame(src, tgt)
    
    names(nodes)[1] <- "nodes"
    
    # order
    nodes[order(nodes$nodes),]
    
  } else {
    
    nodes <- c(unique(as.character(edges[, source])), 
               unique(as.character(edges[, target])))
    
    nodes <- unique(nodes)
    
    nodes <- nodes[order(nodes)]
  }
  
  return(nodes)
  
}
