# global variables to avoid R CMD CHECK note (timeNodes) 
globalVariables(c("start.stamp", "end.stamp"))

# clean handles
cleanHandles <- function(handles) {
  # clean punctuation
  handles <- trimws(handles) # remove white space
  handles <- regmatches(handles, gregexpr("[[:alnum:]]|_|@", handles))
  handles <- lapply(handles, function(x){ paste0(x, collapse = "")})
  handles <- unlist(handles)
  
  # remove @@
  if(length(grep("@", handles))){
    handles <- substring(handles, 2)
  }
  
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

dots2df <- function(x, nm, dots) {
  ret <- lazyeval::lazy_eval(dots, x)
  names(ret) <- nm
  as.data.frame(ret)
}

get_nodes <- function(edges, source, target, nm, dots){
  
  source <- eval(substitute(source, parent.frame()), edges)
  target <- eval(substitute(target, parent.frame()), edges)
  
  args <- dots2df(edges, nm, dots)
  
  if(length(args)) {
    
    src <- cbind.data.frame(source, args) # bind
    
    # remove duplicates
    src <- unique(src) 
    src <- src[!duplicated(src[,1]),]
    
    names(src)[1] <- "nodes" # rename for rbind
    
    # take unique source and target
    tgt <- unique(target)
    tgt <- tgt[!tgt %in% src[, "nodes"]] # remove targets in source
    tgt <- data.frame(nodes = tgt)
    
    nodes <- plyr::rbind.fill(src, tgt)
    
  } else {
    nodes <- unique(c(source, target))
  }
  return(nodes)
}

get_edges <- function(data, tweets, source, str.length, nm, dots){
  
  tweets <- eval(substitute(tweets, parent.frame()), data)
  source <- eval(substitute(source, parent.frame()), data)
  
  # get handles
  handles <- lapply(tweets, function(x) {
    regmatches(x, gregexpr("@[^ ]*", x))[[1]]
  })
  
  source <- lapply(source, function(x) {
    cleanHandles(x)
  })
  
  # cut string
  if(!is.null(str.length)){
    
    source <- substring(source, 0, str.length) # cut source
    
    # cut screenName
    handles <- lapply(handles, function(x) {
      substring(x, 0, str.length)
    })
    
  }
  
  # clean handles
  handles <- lapply(handles, function(x) {
    cleanHandles(x)
  })
  
  # dots
  args <- dots2df(data, nm, dots)
  
  if(length(args)) {
    
    names(handles) <- source
    source <- reshape2::melt(handles)
    
    edges <- list()
    edges[[1]] <- source
    for(i in 1:ncol(args)){
      names(handles) <- args[,i]
      edges[[i+1]] <- reshape2::melt(handles)
    }
    
    edges <- do.call("cbind.data.frame", lapply(edges, as.data.frame))
    
    names(edges)[1:2] <- c("source", "target")
    edges <- edges[,!grepl("value", names(edges))]
    
    # rename 
    names(edges)[3:ncol(edges)] <- names(args)
    
  } else {
    
    names(handles) <- source # name for melt
    
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