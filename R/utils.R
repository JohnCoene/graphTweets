# global variables to avoid R CMD CHECK note (timeNodes) 
globalVariables(c("start.stamp", "end.stamp", ".", "created_at", "target", "end"))

# clean handles
clean_handles <- function(handles) {
  # clean punctuation
  handles <- gsub("[[:space:]]|:|,|;|>|<|?|\\.*", "", handles) # remove white space
  
  # remove @
  if(length(grep("@", handles))){
    handles <- substring(handles, 2)
  }
  
  return(handles)
}

extract_handles <- function(x) {
  regmatches(x, gregexpr("@[^ ]*", x))[[1]]
}

construct <- function(tweets, edges, nodes = NULL){
  
  data <- list(
    tweets = tweets,
    edges = edges
  )
  
  if(!is.null(nodes)){
    data <- list(
      tweets = tweets,
      edges = edges,
      nodes = nodes
    )
  }
  attr(data, "hidden") <- "tweets"
  structure(data, class = "graphTweets")
}

deconstruct <- function(gt){
  list(
    edges = gt[["edges"]],
    nodes = gt[["nodes"]]
  )
}

test_input <- function(gt){
  if (!inherits(gt, "graphTweets")) 
    stop("gt is not of class graphTweets", call. = FALSE)
}

# clean handles
cleanHandles <- function(handles) {
  # clean punctuation
  handles <- gsub("[[:space:]]", "", handles) # remove white space
  handles <- gsub(":", "", handles)
  handles <- gsub(",", "", handles)
  handles <- gsub(";", "", handles)
  handles <- gsub(">", "", handles)
  handles <- gsub("<", "", handles)
  handles <- gsub("?", "", handles)
  handles <- gsub("\\...", "", handles)
  
  # remove @@
  if(length(grep("@", handles))){
    handles <- substring(handles, 2)
  }
  
  handles <- gsub("@", "", handles)
  handles <- trimws(handles)
  
  return(handles)
}

# timeNodes
timeNodes <- function(data){
  
  # split
  src <- data[, c("source", "start.stamp", "end.stamp")]
  tgt <- data[, c("target", "start.stamp", "end.stamp")]
  
  # bind
  names(tgt)[1] <- "source"
  nodes <- rbind.data.frame(src, tgt)
  
  # get earliest time.stamp
  n_grp <- dplyr::group_by(nodes, source)
  n_s <- dplyr::slice(n_grp, which.min(start.stamp))
  n_s <- as.data.frame(n_s)
  
  # get latest
  n_e <- dplyr::slice(n_grp, which.max(end.stamp))
  n_e <- as.data.frame(n_e)
  
  # remove unwanted columns before join
  n_s$end.stamp <- NULL
  n_e$start.stamp <- NULL
  
  # merge
  nodes <- dplyr::inner_join(n_s, n_e, by = "source")
  
  names(nodes)[1] <- c("label")
  
  return(nodes)
  
}