#' Edges
#' 
#' Get edges from data.frame of tweets.
#' 
#' @param data Data.frame of tweets, usually returned by the \code{rtweet} package.
#' @param tweets Column containing tweets.
#' @param source Author of tweets.
#' 
#' @examples 
#' # simulate dataset
#' tweets <- tibble(
#'   text = c("I tweet @you about @him", 
#'            "I tweet @me about @you"),
#'   screen_name = c("me", "him")
#' )
#' 
#' tweets %>% 
#'   gt_edges(text, screen_name)
#' 
#' @rdname edges
#' @export
gt_edges <- function(data, tweets, source, id, ...){
  tweets <- deparse(substitute(tweets))
  source <- deparse(substitute(source))
  gt_edges_(data, tweets, source, meta)
}

#' @rdname edges
#' @export
gt_edges_ <- function(data, tweets = "text", source = "screen_name",  id = "status_id", ...){
  
  handles <- data[, tweets] %>% 
    unlist() %>% 
    purrr::map(., extract_handles) %>% 
    purrr::set_names(unlist(data[, id])) %>% 
    purrr::map(., clean_handles) %>% 
    purrr::map(., paste0, collapse = ",")
  
  df <- tibble(
    handles = unlist(handles),
    status_id = names(handles)
  )
  names(df) <- c("handles", id)
  
  df %>% 
    dplyr::left_join(data, handles, by = id) %>% 
    splitstackshape::cSplit(., "handles", ",", direction = "long") %>% 
    dplyr::select_(
      source = source, 
      handles = "handles",
      ...
      )
  
}