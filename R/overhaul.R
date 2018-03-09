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
#' @rdname edges
#' @export
gt_edges <- function(data, tweets, source, id, ...){
  tweets <- deparse(substitute(tweets))
  source <- deparse(substitute(source))
  gt_edges_(data, tweets, source)
}

#' @rdname edges
#' @export
gt_edges_ <- function(data, tweets = "text", source = "screen_name",  id = "status_id", ...){
  
  if(length(unique(unlist(data[, id]))) != nrow(data))
    stop("id are not unique", call. = FALSE)
  
  handles <- data[, tweets] %>% 
    unlist() %>% 
    purrr::map(., extract_handles) %>% 
    purrr::set_names(unlist(data[, id])) %>% 
    purrr::map(., clean_handles) %>% 
    purrr::map(., paste0, collapse = ",")
  
  df <- dplyr::tibble(
    handles = unlist(handles),
    status_id = names(handles)
  )
  names(df) <- c("handles", id)
  
  if(!inherits(data[, id], "character"))
    data[, id] <- as.character(unlist(data[, id]))
  
  df %>% 
    dplyr::left_join(data, handles, by = id) %>% 
    splitstackshape::cSplit(., "handles", ",", direction = "long") %>% 
    dplyr::mutate(handles = as.character(handles)) %>% 
    dplyr::select_(
      source = source, 
      target = "handles",
      ...
      ) -> edges
  
  construct(data, edges, NULL)
  
}

#' Nodes
#' 
#' @inheritParams gt_collect
#' 
#' @export
gt_nodes <- function(gt, meta = FALSE){
  test_input(gt)
  
  usr <- rtweet::users_data(gt$tweets)
  
  if(isTRUE(meta)){
    gt$edges %>% 
      dplyr::left_join(usr, by = c("handles" = "screen_name")) -> nodes
  } else {
    nodes <- gt$edges
  }
  
  nodes %>% 
    dplyr::group_by_("source", "target")
}

#' Collect
#' 
#' @param gt An object of class \code{graphTweets} as returned by \code{\link{gt_edges}}.
#' 
#' @export
gt_collect <- function(gt){
  test_input(gt)
  
  gt
}