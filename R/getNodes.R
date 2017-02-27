#' Build node table from edges
#' 
#' @description Get nodes from a data.frame of edges as typically returned by 
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
#' @return Returns vector of nodes, if \code{...} passed returns a data frame.
#' 
#' @details One must keep in mind that nodes need to be unique therefore 
#' duplicate values (\code{...}) are dropped. Also, the meta-data (\code{...}), 
#' only applies to the source of edges; \code{NA} are generated for target nodes.
#' 
#' @examples 
#' # simulate dataset
#' tweets <- data.frame(text = c("I tweet @you about @him", 
#'                               "I tweet @me about @you"),
#'                      screenName = c("me", "him"),
#'                      favorited = c(TRUE, FALSE),
#'                      stringsAsFactors = FALSE)
#' 
#' # get edges
#' edges <- getEdges(data = tweets, tweets = "text", source = "screenName", 
#'                   str.length = NULL, "favorited")
#' 
#' # get nodes
#' nodes <- getNodes(edges, source = "source", target = "target", 
#'                   "favorited")
#' 
#' # plot
#' g <- igraph::graph.data.frame(edges, directed = TRUE, vertices = nodes)
#' plot(g, vertex.color = igraph::V(g)$favorited)
#' 
#' @author John Coene \email{jcoenep@@gmail.com}
#' 
#' @export
getNodes <- function(edges, source, target, ...) {
  
  # check inputs
  if (class(edges) != "data.frame") {
    stop("edges must be a data.frame")
  }   
  
  source <- eval(substitute(source), edges)
  target <- eval(substitute(target), edges)
  
  args <- dots2df(edges, ...)
  
  if(length(args)) {
    
    src <- cbind.data.frame(source, args) # bind
    
    # remove duplicates
    src <- unique(src) 
    src <- src[!duplicated(src[,1]),]
    
    names(src)[1] <- "nodes" # rename for rbind
    
    # take unique source and target
    tgt <- unique(target)
    tgt <- tgt[!tgt %in% src[, "nodes"]] # remove targets in source
    tgt <- data.frame(node = tgt)
    
    nodes <- plyr::rbind.fill(src, tgt)
    
  } else {
    nodes <- unique(c(source, target))
  }
  
  return(nodes)
  
}
