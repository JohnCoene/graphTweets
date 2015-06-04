#' edge_table
#' 
#' @description Builds a table of edges (source, target) from a list of tweets by subsetting @@tags from the text.
#' 
#' @usage 
#' 
#' edge_table(tweet_df, text, screenName, strLength = FALSE, ...)
#' 
#' @param tweet_df Data frame of tweets which includes text and screenNames, required.
#' @param text Column name of tweets within tweet_df, must be of character class, required.
#' @param screenName User name or ID column, must be of character class, required.
#' @param strLength Defaults to FALSE. Shorten length of @@tags (see details), maximum number of characters, optional.
#' @param ... Any other columns to be passed on to the edge_table function, optional.
#' 
#' @details
#' 
#' The edges function takes in a data frame of tweets -typically obtained from the twitter Search or Streaming API-, scrapes the content of tweets to subset the @@tags subsequently forming a table of edges. @@tags are subsets of regular expressions between at-signs (@@) and first space (" ").
#' Note that the table of edges returned is meant for a directed graph.
#' If no @@tags are mentioned in the tweets (text) then the user name or ID (screenName) is repeated to form a self-loop. Self-loop can be identified by loop variables (boolean). Failed @@tags i.e.:@@ tag are not caught and will instead produce a self-loop.
#' Node labels can be shortened using the strLength parameters. This is useful for non-latin alphabet where nodes may be wrongly identified. 
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
#' edges_table <- edge_table(tweet_df = tw_table, text = "text", screenName = "screenName", "retweetCount", strLength = FALSE)
#' 
#' # output
#' names(edges_table)
#' [1] "source"    "target"   "loop"    "longitude"    "latitude" 
#' 
#' }
edge_table <- function(tweet_df, text, screenName, strLength = FALSE, ...) {
  if (class(tweet_df) != "data.frame") {
    stop("tweet_df is not data.frame")
  } else if (missing(text)) {
    stop("missing text column")
  } else if (missing(screenName)) {
    stop("missing screenName column")
  } else if (class(tweet_df[, text]) != "character"){
    stop("text must be of class character")
  } else if (class(tweet_df[, screenName]) != "character"){
    stop("ScreenName must be of class character")
  } else if (strLength != FALSE & class(strLength) != "numeric") {
    stop("strLength must be numeric")
  }
  edges <- data.frame()
  edge_tb <- data.frame()
  text <- tweet_df[, text]
  screenName <- tweet_df[, screenName]
  if(missing(strLength)){
  } else {
    screenName <- substring(screenName, 0, strLength)
  }
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
      if(strLength == FALSE){
        
      } else {
        handles <- substring(handles, 0, strLength)
      }
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
        names(edge_tb) <- c("source", "target", args)
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
        names(edge_tb) <- c("source", "target", args)
      } else {
        edge_tb <- edge_table
      }
      names(edge_tb)[1:2] <- c("source", "target")
    }
    edges <- rbind(edge_tb, edges)
  }
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
#' @param edge_table data.frame. Assumes first column is source node, second is target node, required.
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
#' # output
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
    edges_args <- as.data.frame(edge_table[,args])
    names(edges_args) <- args
    edges_src <- cbind(edge_table[1], edges_args)
    edges_tg <- cbind(edge_table[2], edges_args)
    names(edges_tg)[1] <- "source"
    edg_db <- rbind(edges_src, edges_tg)
    edg_db[(nrow(edges_tg)+1):nrow(edg_db), args] <- NA
    edg_db <- edg_db[!duplicated(edg_db[,args,]),]
    nodes <- left_join(nodes, edg_db, by = "source")
    names(nodes) <- c("screenName", args)
  } else {
    names(nodes) <- "screenName"
  }
  nodes <- nodes[!duplicated(nodes[c("screenName")]), ]
  return(nodes)
}