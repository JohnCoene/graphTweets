#' edge_table
#' 
#' @description Builds a table of edges (source, target) from a list of tweets by subsetting @@tags from the text.
#' 
#' @usage 
#' 
#' edge_table(tweet_df, text, screenName, ...)
#' 
#' @param tweet_df Required. Data frame of tweets
#' @param text Required. Column name of tweets within tweet_df
#' @param screenName Required. User name or ID column.
#' @param ... Any other columns to be passed on to the edge_table, otpional.
#' 
#' @details
#' 
#' The edges function takes in a data frame of tweets -typically obtained from the twitter Search or Streaming API-, scrapes the content of tweets to subset the @@tags subsequently forming a table of edges. @@tags are subsets of regular expressions between at-signs (@@) and first space (" ").
#' Note that the table of edges returned is meant for a directed graph.
#' If no @@tags are mentioned in the tweets (text) then the user name or ID (screenName) is repeated to form a self-loop. Self-loop can be identified by loop variables (boolean). Failed @@tags i.e.:@@ tag are not caught and will instead produce a self-loop.
#' 
#' @return
#' 
#' Returns a table of edges; source, target, loop, ...
#' 
#' @seealso
#' 
#' \href{http://cran.r-project.org/web/packages/twitteR/twitteR.pdf}{twitteR} and \href{http://cran.r-project.org/web/packages/streamR/streamR.pdf}{streamR} packages wherefrom the data (tweets_df) can be obtained.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' names(tw_table)
#' [1] "text"          "favorited"     "favoriteCount" "replyToSN"     "created"      
#' [6] "truncated"     "replyToSID"    "id"            "replyToUID"    "statusSource" 
#' [11] "screenName"    "retweetCount"  "isRetweet"     "retweeted"     "longitude"    
#' [16] "latitude" 
#' 
#' edges_table <- edge_table(tweet_df=tw_table, text="text", screenName="screenName", "longitude", "latitude")
#' 
#' names(edges_table)
#' [1] "source"    "target"    "longitude" "latitude" 
#' 
#' }
edge_table <- function(tweet_df, text, screenName, ...) {
  if (class(tweet_df) != "data.frame") {
    stop("tweet_df is not data.frame")
  } else if (missing(text)) {
    stop("missing text column")
  } else if (missing(screenName)) {
    stop("missing screenName column")
  }
  edges <- data.frame()
  edge_tb <- data.frame()
  text <- tweet_df[, text]
  screenName <- tweet_df[, screenName]
  args <- unlist(list(...))
  for (i in 1:nrow(tweet_df)) {
    handles <- regmatches(text[i], gregexpr("@[^ ]*", text[i]))[[1]]
    handles <- gsub(":", "",handles)
    handles <- gsub(",", "",handles)
    handles <- gsub(";", "",handles)
    handles <- gsub(">", "",handles)
    handles <- gsub("<", "",handles)
    handles <- substring(handles, 2)
    if (length(handles) >= 1) {
      for (x in 1:length(handles)){
        if(handles[x] == "") {
          handles[x] <- as.character(screenName[i])
        }
      }
      src <- rep(as.character(screenName[i]), length(handles))
      edge_table <- as.data.frame(cbind(as.character(src), as.character(handles)))
      if(length(args)) {
        meta <- as.data.frame(tweet_df[i, args])
        meta <- meta[rep(seq_len(nrow(meta)), each=length(handles)),]
        edge_tb <- cbind(edge_table, meta)
      } else {
        edge_tb <- edge_table
      }
      names(edge_tb)[1:2] <- c("source", "target")
    } else {
      edge_table <- as.data.frame(cbind(as.character(screenName[i]),
                                        as.character(screenName[i])))
      if( length(args)) {
        meta <- tweet_df[i, args]
        edge_tb <- cbind(edge_table, meta)
      } else {
        edge_tb <- edge_table
      }
      names(edge_tb)[1:2] <- c("source", "target")
    }
    edges <- rbind(edge_tb, edges)
  }
  edges <- unique(edges)
  edges <- edges[complete.cases(edges),]
  edges$loop <- FALSE
  for(i in 1:nrow(edges)){
    if( as.character(edges$source[i] == as.character(edges$target[i]))){
      edges$loop[i] <- TRUE
    } else {
      edges$loop[i] <- FALSE
    }
  }
  return(edges)
}

#' node_table
#' 
#' @description Builds table of nodes|vertices from an edge table (data.frame). Use to metadata to igraph objects/graphml files.
#' 
#' 
#' @usage
#' 
#' node_table(edge_table, ...)
#' 
#' @param edge_table Required - data.frame. Assumes first column is source node, second is target node.
#' @param ... Any other column names (meta-data)
#' 
#' @return Returns table of nodes/vertices; first column is nodes' name or ID following columns are optional args. Meant to be used as meta-data for graph.
#' 
#' @details Looks for uniques in both source and target columns of the edge table it is fed. Note that other args (optional) only apply to the source node; will return NA for target nodes. Assumes first column of the edge_table are source nodes and the second are target nodes.
#' 
#' @seealso \code{\link{edge_table}}
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' names(edge_table)
#' [1] "source"    "target"    "longitude" "latitude"  "self_loop"
#' 
#' node_table <- node_table(edge_table, "longitude", "latitude")
#' 
#' names(node_table)
#' [1] "screenName" "longitude"  "latitude"
#' }
node_table <- function(edge_table, ...) {
  if (class(edge_table) != "data.frame") {
    stop("edge_table must be data.frame")
  } else if (missing(edge_table)) {
    stop("missing edge_table")
  }
  names(edge_table)[2] <- "source"
  nodes <- as.data.frame(unique(rbind(edge_table[1], edge_table[2])))
  args <- unlist(list(...))
  if(length(args)) {
    edges_args <- edge_table[,args]
    edges_src <- cbind(edge_table[1], edges_args)
    edges_tg <- cbind(edge_table[2], edges_args)
    names(edges_tg)[1] <- "source"
    edg_db <- unique(rbind(edges_src, edges_tg))
    edg_db[nrow(edges_tg):nrow(edg_db), args] <- NA
    library(dplyr)
    nodes <- left_join(nodes, edg_db, by = "source")
    names(nodes) <- c("screenName", args)
  } else {
    names(nodes) <- "screenName"
  }
  nodes <- nodes[!duplicated(nodes[c("screenName")]), ]
  return(nodes)
}