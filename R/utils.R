# global variables to avoid R CMD CHECK note (timeNodes) 
globalVariables(
  c(
    "start.stamp", "end.stamp", ".", "created_at", "target", "end", "type",
    "status_id"
  )
)

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

construct <- function(gt, tweets, edges, nodes = NULL){
  
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

.select_edges <- function(data, source, target, ...){
  data %>% 
    dplyr::select(source = !!source, target = !!target, ...) %>% 
    tidyr::unnest(target) 
}

.rename_targets <- function(edges, col_name){
  edges %>% 
    dplyr::mutate(
      target = dplyr::case_when(
        col_name == "hashtags" ~ paste0("#", target),
        TRUE ~ target
      )
    )
}

.rename_sources <- function(edges, col_name){
  edges %>% 
    dplyr::mutate(
      source = dplyr::case_when(
        col_name == "hashtags" ~ paste0("#", source),
        TRUE ~ source
      )
    )
}

.get_edges <- function(edges, tl, ...){
  edges %>% 
    dplyr::mutate(
      target = dplyr::case_when(
        tl == TRUE ~ tolower(target),
        TRUE ~ target
      ),
      source = dplyr::case_when(
        tl == TRUE ~ tolower(source),
        TRUE ~ source
      )
    ) %>% 
    dplyr::group_by(source, target, ...) %>% 
    dplyr::count() %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(!is.na(target)) 
}

append_graph <- function(gt, tweets, edges, nodes = NULL){
  
  if(length(gt$edges) > 1)
    gt$edges <- dplyr::bind_rows(gt$edges, edges)
  
  if(length(gt$nodes) > 1)
    gt$nodes <- dplyr::bind_rows(gt$nodes, nodes)
  
  if(length(gt$tweets) > 1){
    if(sum(!tweets$status_id %in% tweets$status_id) >= 1){
      tw <- tweets %>% 
        dplyr::filter(status_id %in% tweets$status_id)
      
      gt$tweets <- dplyr::bind_rows(gt$tweets, tw)
    }
  }
  
  return(gt)
}

.bind_co_occurences <- function(x){
  h <- unlist(x)
  if(length(h) > 1){
    cbn <- combinat::combn(h, 2, simplify = FALSE) %>% 
      purrr::map(function(n){
        names(n) <- c("source", "target")
        return(n)
      }) %>% 
      purrr::map_df(dplyr::bind_rows)
    return(cbn)
  }
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