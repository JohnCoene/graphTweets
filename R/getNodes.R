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
#' @details One must keep in mind that nodes need to be unique therefore 
#' duplicate values (\code{...}) are dropped. Also, the meta-data (\code{...}), 
#' only applies to the source of edges; NAs are generated for target nodes.
#' 
#' @examples 
#' \dontrun{
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
#' }
#' 
#' @author John Coene \email{jcoenep@@gmail.com}
#' 
#' @export
getNodes <- function(edges, source = "source", target = "target", ...) {
  
  .Deprecated("gt_nodes")
  
  # check inputs
  if (class(edges) != "data.frame") {
    stop("edges must be a data.frame")
  } 
  
  if (!source %in% names(edges)) {
    stop(paste0("no column named '", source, "' found in data"))
  } 
  
  if (!target %in% names(edges)) {
    stop(paste0("no column named '", target, "' found in data"))
  }
  
  args <- unlist(list(...))
  
  if(length(args)) {
    
    # split
    src <- unique(edges[, c(source, args)])
    tgt <- data.frame(target = unique(edges[, target]))
    
    # remove duplicates
    src <- src[!duplicated(src[, source]),]
    
    # to character
    src[, source] <- as.character(src[, source])
    tgt[, target] <- as.character(tgt[, target])
    
    # anti_join
    tgt <- dplyr::anti_join(tgt, src, by = c(target = source))
    
    # adds args
    tgt[, args] <- NA
    
    # rename for bind
    names(tgt)[1] <- source
    
    # bind
    nodes <- rbind.data.frame(src, tgt)
    
    names(nodes)[1] <- "nodes"
    
    # order
    nodes <- nodes[order(nodes$nodes),]
    
  } else {
    
    nodes <- c(unique(as.character(edges[, source])), 
               unique(as.character(edges[, target])))
    
    nodes <- unique(nodes)
    
    nodes <- nodes[order(nodes)]
    
    nodes <- data.frame(nodes = nodes)
  }
  
  return(nodes)
  
}
