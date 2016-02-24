#' getEdges
#' 
#' @description Builds a table of edges (source, target) from a list of tweets 
#' by subsetting @@tags from the text.
#' 
#' @param data \code{data.frame} of tweets, typically returned by 
#' \code{\link[twitteR]{searchTwitter}}, required
#' @param tweets Column name of tweets within \code{data}, must be a 
#' \code{character}, required.
#' @param source User names or ID column of \code{tweets} author, must be a 
#' \code{character}, required.
#' @param str.length Defaults to \code{NULL}. Shorten length of @@tags 
#' (see details), to a maximum number of characters, optional.
#' @param ... Any other columns to be passed on to the edges, 
#' optional.
#' 
#' @details The edges function takes in a data frame of tweets, typically 
#' obtained from the twitter Search or Streaming API, scrapes the content of 
#' tweets to subset the @@tags subsequently forming a table of edges. @@tags 
#' are subsets of regular expressions between at-signs (@@) and first 
#' space (" "). 
#' Note that the table of edges returned is meant for a directed graph.
#' Node labels can be shortened using the strLength parameters. 
#' This is useful for non-latin alphabet where nodes may be wrongly identified. 
#' 
#' @seealso \href{http://cran.r-project.org/web/packages/twitteR/twitteR.pdf}{twitteR} 
#' and \href{http://cran.r-project.org/web/packages/streamR/streamR.pdf}{streamR} packages wherefrom the data (tweets_df) can be obtained.
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
#' # get edges with coordinates
#' edges <- getEdges(data = tweets, tweets = "text", source = "screenName", 
#'                   "longitude", "latitude")
#'                   
#' # load igraph
#' library(igraph)
#' 
#' # plot
#' g <- graph.data.frame(edges, directed=TRUE)
#' 
#' plot(g)
#' }
#' 
#' @author John Coene \email{john.coene@@gmail.com}
#' 
#' @export 
getEdges <- function(data, tweets, source, str.length = NULL, ...) {
  
  if (class(data) != "data.frame") {
    stop("data must be a data.frame")
  } 
  
  if (missing(tweets)) {
    stop("missing tweets column")
  } else if (missing(source)) {
    stop("missing source column")
  } else if (class(data[, tweets]) != "character"){
    stop("text must be of class character")
  } else if (class(data[, source]) != "character"){
    stop("ScreenName must be of class character")
  } else if (!is.null(str.length) && class(str.length) != "numeric") {
    stop("strLength must be numeric")
  }
  
  # cut source
  if(!is.null(str.length)){
    
    # cut screenName
    data[, source] <- substring(data[, source], 0, str.length)
    
  }
  
  # get handles
  handles <- lapply(data[, tweets], function(x) {
    regmatches(x, gregexpr("@[^ ]*", x))[[1]]
  })
  
  # clean handles
  handles <- lapply(handles, function(x) {
    cleanHandles(x)
  })
  
  # cut string
  if(!is.null(str.length)){
    
    # cut screenName
    handles <- lapply(handles, function(x) {
      substring(x, 0, str.length)
    })
    
  }
  
  source <- lapply(data[, source], function(x) {
    cleanHandles(x)
  })
  
  # name handles (indicate source)
  names(handles) <- source
  
  # additional arguments
  # get arguments
  args <- unlist(list(...))
  
  if(length(args)) {
    
    ext <- as.data.frame(data[, args])
    
    edges <- data.frame()
    
    for(i in 1:length(handles)) {
      
      if(length(handles[[i]])) {
        
        sub <- reshape2::melt(handles[i])
        
        # reorder source and target
        sub <- sub[,c(2,1)]
        
        # rename
        names(sub) <- c("source", "target")
        
        sub_edges <- cbind.data.frame(sub, ext[i,], row.names = NULL)
        
        edges <- rbind.data.frame(edges, sub_edges)
      }
    }
    
    # rename 
    names(edges)[3:ncol(edges)] <- args
    
  } else {
    
    # to edge data.frame
    edges <- reshape2::melt(handles)
    
    # reorder source and target
    edges <- edges[,c(2,1)]
    
    # rename
    names(edges) <- c("source", "target")
  }
  
  return(edges)
}