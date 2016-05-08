#' Make a dynamic graph
#' 
#' @description Create a dynamic graph from tweets and, optionally, open it in 
#' \href{https://gephi.org/}{Gephi}
#' 
#' @param data \code{data.frame} of tweets, typically returned by 
#' \code{\link[twitteR]{searchTwitter}}, required.
#' @param tweets Column name of tweets within \code{data}, must be a 
#' \code{character} string, required.
#' @param source User names or ID column of \code{tweets} author, must be a 
#' \code{character} string, required.
#' @param start.stamp Typically a date or time, but may 
#' also be an interger or a factor, cannot be a character.
#' @param end.stamp The end of the time stamp, or when edges are to leave the 
#' graph, defaults to \code{NULL} (edges never disappear). See details.
#' @param str.length Defaults to \code{NULL}. Shorten length of @@tags 
#' (see details in \code{\link{getEdges}}), to a maximum number of characters, 
#' optional.
#' @param write if \code{TRUE} saves graph as file.
#' @param format if \code{write = TRUE} set format of file, defaults to 
#' \code{graphml} (see details for valid formats).
#' @param file.dir if \code{write = TRUE} directory where to save the file, 
#' defaults to working directory.
#' @param file.name if \code{write = TRUE} name of file
#' @param open if \code{write = TRUE}, \code{open = TRUE} opens file in 
#' \href{Gephi}{https://gephi.org/}.
#' 
#' @details \code{end.stamp}: When the edges are to disappear (consider lifetime 
#' of a tweet), by default edges stay on the graph forever.
#' 
#' Valid values for \code{format}:
#' \itemize{
#' \item \code{edgelist}
#' \item \code{pajek}
#' \item \code{ncol}
#' \item \code{lgl}
#' \item \code{graphml} (default)
#' \item \code{dimacs}
#' \item \code{gml}
#' \item \code{dot}
#' \item \code{leda}
#' }
#' 
#' @examples 
#' tweets <- data.frame(text = c("I tweet @you about @him",
#'                               "I tweet @me about @you"),
#'                        screenName = c("me", "him"),
#'                        created = as.Date(c("2016-01-01", "2016-02-02")),
#'                        stringsAsFactors = FALSE)
#' 
#' # create dynamic graph
#' dyn <- dynamise(tweets, tweets = "text", source = "screenName", 
#'                 start.stamp = "created")
#'                 
#' # create dynamic graph with 60 min lasting edges
#' dyn <- dynamise(tweets, tweets = "text", source = "screenName", 
#'                 start.stamp = "created", end = 3600)
#' 
#' @export
#' 
#' @importFrom methods is
#' 
#' @author John Coene \email{jcoenep@@gmail.com}
dynamise <- function(data, tweets, source, start.stamp, end.stamp = NULL, 
                     str.length = NULL, write = FALSE, format = "graphml", 
                     file.dir = getwd(), file.name = "graphTweets", 
                     open = FALSE){
  
  # check inputs
  if(!is(data, "data.frame")){
    stop("data must be a data.frame")
  }
  
  if(!tweets %in% names(data)) {
    stop(paste0("tweets: cannot find column named '", tweets, "' in data"))
  }
  
  if(!source %in% names(data)) {
    stop(paste0("source: cannot find column named '", source, "' in data"))
  }
  
  if(!start.stamp %in% names(data)) {
    stop(paste0("start.stamp: cannot find column named '", start.stamp, 
                "' in data"))
  }
  
  if(class(data[, start.stamp])[1] == "character") {
    stop("start.stamp cannot be characters")
  }
  
  if(open == TRUE && write == FALSE) {
    warning("cannot open file since write = FALSE")
  }
  
  
  
  if(!is.null(end.stamp)){
    
    if(is.numeric(end.stamp)){
      
      edges <- graphTweets::getEdges(data, tweets = tweets, source = source,
                                     str.length = str.length, start.stamp)
      
      names(edges) <- c("source", "target", "start.stamp")
      
      edges$end.stamp <- edges$start.stamp + end.stamp
      
    } else if(!is.numeric(end.stamp) && !is.factor(end.stamp)){
      
      x <- tryCatch(data[,end.stamp], error = function(e) e)
      
      if(is(x, "error")){
        stop("cannot find column ", end.stamp)
      }
      
      if(class(data[, start.stamp])[1] != class(data[, end.stamp])[1]) {
        stop(paste0("start.stamp and end.stamp are of different classes,", 
                    " start.stamp: ", 
                    paste0(class(data[, start.stamp]), collapse = " "),
                    " while end.stamp: ", 
                    paste0(class(data[, end.stamp]), collapse = " ")))
      }
      
      edges <- graphTweets::getEdges(data, tweets = tweets, source = source,
                                     str.length = str.length, start.stamp, 
                                     end.stamp)
      
      names(edges) <- c("source", "target", "start.stamp", "end.stamp")
      
    }
    
  } else if (is.null(end.stamp)){
    
    edges <- graphTweets::getEdges(data, tweets = tweets, source = source,
                                   str.length = str.length, start.stamp)
    
    names(edges) <- c("source", "target", "start.stamp")
    
    edges$end.stamp <- max(edges$start.stamp)
    
  }

  # get nodes
  nodes <- timeNodes(data = edges)
  
  # stamps to charatcter, igraph doesn't like dates
  edges$start.stamp <- as.character(edges$start.stamp)
  edges$end.stamp <- as.character(edges$end.stamp)
  nodes$start.stamp <- as.character(nodes$start.stamp)
  nodes$end.stamp <- as.character(nodes$end.stamp)
  
  # build graph
  g <- igraph::graph.data.frame(edges, directed = TRUE, vertices = nodes)
  
  if(write == TRUE){
    
    path <- file.path(normalizePath(file.dir), 
                      paste(file.name, ".", format, sep=""))
    
    igraph::write.graph(g, file = path, format = format)
    
    if(open == TRUE) {
      
      op <- paste0("open ", path)
      
      system(op)
    }
    
  } 
  
  return(g)
}