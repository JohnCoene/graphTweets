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

dots2df <- function(x, ...) {
  dots <- lazyeval::lazy_dots(...)
  ret <- lazyeval::lazy_eval(dots, x)
  names(ret) <- eval(substitute(alist(...)))
  as.data.frame(ret)
}