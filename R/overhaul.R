#' Edges
#' 
#' Get edges from data.frame of tweets.
#' 
#' @param data Data.frame of tweets, usually returned by the \code{rtweet} package.
#' @param tweets Column containing tweets.
#' @param source Author of tweets.
#' @param id tweets unique id.
#' @param ... any other column name, see examples.
#' 
#' @examples 
#' # simulate dataset
#' tweets <- data.frame(
#'   text = c("I tweet @you about @him", 
#'            "I tweet @me about @you"),
#'   screen_name = c("me", "him"),
#'   retweet_count = c(19, 5),
#'   status_id = c(1, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' tweets %>% 
#'   gt_edges(text, screen_name)
#'   
#' tweets %>% 
#'   gt_edges_(RT = "retweet_count") # metadata
#'   
#' @return An object of class \code{graphTweets}.
#' 
#' @rdname edges
#' @export
gt_edges <- function(data, tweets, source, ...){
  tweets <- dplyr::enquo(tweets)
  source <- dplyr::enquo(source)
  gt_edges_(data, tweets, source, "status_id", ...)
}

#' @rdname edges
#' @export
gt_edges_ <- function(data, tweets = "text", source = "screen_name",  id = "status_id", ...){
  
  ids <- data[[id]]
  
  if(length(unique(ids)) != nrow(data))
    stop("id are not unique", call. = FALSE)
  
  handles <- data %>%
    dplyr::select(!!tweets) %>% 
    unlist() %>% 
    purrr::map(., extract_handles) %>% 
    purrr::set_names(ids) %>% 
    purrr::map(., clean_handles) %>% 
    purrr::map(., paste0, collapse = ",")
  
  df <- dplyr::tibble(
    handles = unlist(handles),
    status_id = names(handles)
  )
  names(df) <- c("handles", id)
  
  if(!inherits(ids, "character"))
    data[[id]] <- as.character(ids)
  
  df %>% 
    dplyr::left_join(data, handles, by = id) %>% 
    splitstackshape::cSplit(., "handles", ",", direction = "long") %>% 
    dplyr::mutate(handles = as.character(handles)) %>% 
    dplyr::select_(
      source = source, 
      target = "handles",
      ...
      ) %>% 
    dplyr::as_tibble()-> edges
  
  construct(data, edges, NULL)
  
}

#' Nodes
#' 
#' Get nodes from a \code{graphTweets} object.
#' 
#' @inheritParams gt_collect
#' @param meta Set to \code{TRUE} to add meta data to nodes.
#' 
#' @examples 
#' # simulate dataset
#' tweets <- data.frame(
#'   text = c("I tweet @you about @him", 
#'            "I tweet @me about @you"),
#'   screen_name = c("me", "him"),
#'   retweet_count = c(19, 5),
#'   status_id = c(1, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' tweets %>% 
#'   gt_edges(text, screen_name) %>% 
#'   gt_nodes() -> net
#'   
#' @return An object of class \code{graphTweets}.
#' 
#' @export
gt_nodes <- function(gt, meta = FALSE){
  
  test_input(gt)
  
  nodes <- unique(c(gt[["edges"]][["source"]], gt[["edges"]][["target"]]))
  nodes <- dplyr::tibble(
    nodes = nodes
  )
  
  if(isTRUE(meta)){
    usr <- rtweet::users_data(gt$tweets)
    
    nodes %>% 
      dplyr::left_join(usr, by = c("nodes" = "screen_name")) -> nodes
  } 
  
  construct(gt[["tweets"]], gt[["edges"]], nodes)
}

#' Collect
#' 
#' @param gt An object of class \code{graphTweets} as returned by \code{\link{gt_edges}}.
#' 
#' 
#' @return A list of 1) edges and 2) nodes as \link[dlyr]{tibble}. 
#' 
#' @export
gt_collect <- function(gt){
  test_input(gt)
  
  deconstruct(gt)
}