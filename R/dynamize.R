#' dynamise
#' 
#' @description Create dynamic graph from tweets and, optionally, open it in 
#' \href{Gephi}{https://gephi.org/}
#' 
#' @param data \code{data.frame} of tweets, typically returned by 
#' \code{\link[twitteR]{searchTwitter}}, required.
#' @param tweets Column name of tweets within \code{data}, must be a 
#' \code{character} string, required.
#' @param source User names or ID column of \code{tweets} author, must be a 
#' \code{character} string, required.
#' @param start.stamp time.stamp to dynamise, typically a date or time, but may 
#' also be an interger or a factor, cannot be a character.
#' @param end.stamp The end of the time stamp, or when edges are to leave the 
#' graph, defaults to \code{NULL} (edges stay). See details.
#' @param str.length Defaults to \code{NULL}. Shorten length of @@tags 
#' (see details), to a maximum number of characters, optional.
#' @param write if \code{TRUE} save graph as file.
#' @param format if \code{write = TRUE} set format of file, defaults to 
#' \code{graphml} (see details for valid formats).
#' @param file.dir if \code{write = TRUE} directory where to save the file, 
#' defaults to working directory.
#' @param file.name if \code{write = TRUE} name of file
#' @param open if \code{write = TRUE}, \code{open = TRUE} opens file in 
#' \href{Gephi}{https://gephi.org/}.
#' 
#' @details \code{end.stamp} how long after an edge appears (tweets is made) is 
#' it disapearing.
#' 
#' Valid values for \code{format}:
#' \itemize{
#' \item \code{edgelist}
#' \item \code{pajek}
#' \item \code{ncol}
#' \item \code{lgl}
#' \item \code{graphml}
#' \item \code{dimacs}
#' \item \code{gml}
#' \item \code{dot}
#' \item \code{leda}
#' }
#' 
#' @examples 
#' \dontrun{
#' # load sample data (200 tweets on #rstats)
#' data("tweets")
#' 
#' # create dynamic graph
#' dyn <- dynamise(tweets, tweets = "text", source = "screenName", 
#'                 time.stamp = "created")
#'                 
#' # create dynamic graph and open in Gephi
#' dyn <- dynamise(tweets, tweets = "text", source = "screenName", 
#'                 time.stamp = "created", write = TRUE, open = TRUE)
#' }
#' 
#' @export
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
  
  if(!length(data[, tweets])) {
    stop("invalid tweets column")
  }
  
  if(!length(data[, source])) {
    stop("invalid source column name")
  }
  
  if(!length(data[, start.stamp])) {
    stop("cannot find start.stamp columns")
  }
  
  if(class(data[, start.stamp])[1] == "character") {
    stop("start.stamp cannot be characters")
  }
  
  if(open == TRUE && write == FALSE) {
    warning("cannot open file as write = FALSE")
  }
  
  edges <- graphTweets::getEdges(data, tweets = tweets, source = source,
                                 str.length = str.length, start.stamp)
  
  names(edges) <- c("source", "target", "start.stamp")
  
  # add end time
  if(is.null(end.stamp)){
    
    edges$end.stamp <- max(edges$start.stamp)
    
  } else {
    
    edges$end.stamp <- edges$start.stamp + end.stamp
    
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