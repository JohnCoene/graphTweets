#' Build list of edges from tweets
#' 
#' @description Builds a table of edges (source, target) from a data.frame 
#' of tweets by subsetting @@tags from the text.
#' 
#' @param data \code{data.frame} of tweets, typically returned by 
#' \code{\link[twitteR]{searchTwitter}}, required.
#' @param tweets Column name of tweets within \code{data}, must be a 
#' \code{character} string, required.
#' @param source User names or ID column of \code{tweets} author, must be a 
#' \code{character} string, required.
#' @param str.length Defaults to \code{NULL}. Shorten length of @@tags 
#' (see details), to a maximum number of characters.
#' @param ... Any other columns to be passed on to the edges.
#' 
#' @details The edges function takes in a data frame of tweets, typically 
#' obtained from the twitter Search or Streaming API, scrapes the content of 
#' tweets to subset the @@tags subsequently forming a table of edges. @@tags 
#' are subsets of regular expressions between at-signs (@@) and first 
#' space (" "). 
#' Note that the table of edges returned is meant for a directed graph.
#' Node labels can be shortened using the \code{str.length} parameters. 
#' This is useful for non-latin alphabet "tweets" where nodes may be wrongly identified 
#' (i.e.: Chinese Sina Weibo data). 
#' 
#' @seealso \href{http://cran.r-project.org/web/packages/twitteR/twitteR.pdf}{twitteR} 
#' and \href{http://cran.r-project.org/web/packages/streamR/streamR.pdf}{streamR} 
#' packages wherefrom the data (\code{data}) can be obtained.
#' 
#' @examples 
#' # simulate dataset
#' tweets <- data.frame(text = c("I tweet @you about @him", 
#'                               "I tweet @me about @you"),
#'                      screenName = c("me", "him"),
#'                      stringsAsFactors = FALSE)
#' 
#' # get edges
#' edges <- getEdges(data = tweets, tweets = "text", source = "screenName")
#' 
#' # use igraph to make graph object
#' g <- igraph::graph.data.frame(edges)
#' plot(g)
#' 
#' @author John Coene \email{john.coene@@gmail.com}
#' 
#' @export 
getEdges <- function(data, tweets, source, str.length = NULL, ...) {
  
  tweets <- eval(substitute(tweets), data)
  source <- eval(substitute(source), data)
  
  if (!length(tweets)) {
    stop(paste0("tweets: no column named '", tweets, "' found in data"))
  } else if (!length(source)) {
    stop(paste0("source: no column named '", source, "' found in data"))
  }
  
  if (missing(tweets)) stop("missing tweets column")
  if (missing(source)) stop("missing source column")
  if (class(data[, tweets]) != "character") stop("tweets must be character")
  if (class(data[, source]) != "character") stop("source must be of class character")
  if (!is.null(str.length) && class(str.length) != "numeric") stop("str.length must be numeric") 
  
  # cut string
  if(!is.null(str.length)){
    
    source <- substring(source, 0, str.length) # cut source
    
    # cut screenName
    handles <- lapply(handles, function(x) {
      substring(x, 0, str.length)
    })
    
  }
  
  source <- lapply(source, function(x) {
    cleanHandles(x)
  })
  
  # get handles
  handles <- lapply(tweets, function(x) {
    regmatches(x, gregexpr("@[^ ]*", x))[[1]]
  })
  
  # clean handles
  handles <- lapply(handles, function(x) {
    cleanHandles(x)
  })
  
  names(handles) <- source
  
  # dots
  args <- dots2df(data, ...)
  
  if(length(args)) {
    
    ext <- as.data.frame(data[, c(args)])
    
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
  
  edges$source <- as.character(edges$source)
  edges$target <- as.character(edges$target)
  
  return(edges)
}