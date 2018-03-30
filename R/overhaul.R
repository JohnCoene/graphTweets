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
#'   gt_edges(text, screen_name, status_id)
#'   
#' tweets %>% 
#'   gt_edges_(RT = "retweet_count") # metadata
#'   
#' @return An object of class \code{graphTweets}.
#' 
#' @rdname edges
#' @export
gt_edges <- function(data, tweets, source, id, ...){
  if(missing(data))
    stop("missing data", call. = FALSE)
  tweets <- dplyr::enquo(tweets)
  source <- dplyr::enquo(source)
  id <- deparse(substitute(id))
  gt_edges_(data, tweets, source, id, ...)
}

#' @rdname edges
#' @export
gt_edges_ <- function(data, tweets = "text", source = "screen_name",  id = "status_id", ...){
  
  if(missing(data))
    stop("missing data", call. = FALSE)
  
  if(!inherits(data, "data.frame"))
    stop("data is not of class data.frame")
  
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
#'   gt_edges(text, screen_name, status_id) %>% 
#'   gt_nodes() -> net
#'   
#' @return An object of class \code{graphTweets}.
#' 
#' @export
gt_nodes <- function(gt, meta = FALSE){
  
  if(missing(gt))
    stop("missing gt", call. = FALSE)
  
  test_input(gt)
  
  nodes <- unique(c(gt[["edges"]][["source"]], gt[["edges"]][["target"]]))
  nodes <- dplyr::tibble(
    nodes = nodes
  )
  
  if(isTRUE(meta)){
    usr <- rtweet::users_data(gt$tweets)
    
    nodes %>% 
      dplyr::left_join(usr, by = c("nodes" = "screen_name")) %>% 
      unique() -> nodes
  } 
  
  construct(gt[["tweets"]], gt[["edges"]], nodes)
}

#' Collect
#' 
#' @param gt An object of class \code{graphTweets} as returned by \code{\link{gt_edges}}.
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
#'   gt_edges(text, screen_name, status_id) %>% 
#'   gt_nodes() %>% 
#'   gt_collect() -> net
#' 
#' @return A named list of \link[dplyr]{tibble} 1) edges and 2) nodes. 
#' 
#' @export
gt_collect <- function(gt){
  test_input(gt)
  
  deconstruct(gt)
}

#' Graph
#' 
#' @inherit gt_collect
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
#'   gt_edges(text, screen_name, status_id) %>% 
#'   gt_nodes() %>% 
#'   gt_graph() -> net
#' 
#' @return An object of class \code{igraph}.
#' 
#' @export
gt_graph <- function(gt){
  test_input(gt)
  
  if(!"nodes" %in% names(gt)){
    igraph::graph.data.frame(gt[["edges"]], directed = TRUE)
  } else {
    igraph::graph.data.frame(gt[["edges"]], directed = TRUE, vertices = gt[["nodes"]])
  }
  
}

#' Dynamise
#' 
#' Create a dynamic graph to import in Gephi.
#' 
#' @inheritParams gt_collect
#' @param lifetime Lifetime of a tweet in milliseconds, defaults to \code{Inf}.
#' 
#' @examples 
#' \dontrun{
#' # simulate dataset
#' tweets <- data.frame(
#'   text = c("I tweet @you about @him", 
#'            "I tweet @me about @you"),
#'   screen_name = c("me", "him"),
#'   retweet_count = c(19, 5),
#'   created_at = c(Sys.time(), Sys.time() + 15000),
#'   status_id = c(1, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' tweets %>% 
#'   gt_edges(text, screen_name, status_id, "created_at") %>% 
#'   gt_nodes(TRUE) %>% 
#'   gt_dyn() -> net
#' }
#' 
#' @rdname dyn
#' @export
gt_dyn <- function(gt, lifetime = Inf){
  test_input(gt)
  
  if(!"created_at" %in% names(gt[["edges"]]))
    stop("missing created_at column", call. = FALSE)
  
  if(is.infinite(lifetime)){
    lifetime <- max(gt[["edges"]][["created_at"]])
    gt[["edges"]][["end"]] <- lifetime
  } else {
    gt[["edges"]][["end"]] <- gt[["edges"]][["created_at"]] + lifetime 
  }
  
  src <- gt[["edges"]][, c("source", "created_at")]
  tgt <- gt[["edges"]][, c("target", "created_at")]
  
  names(tgt)[1] <- c("source")
  
  nodes <- src %>% 
    dplyr::bind_rows(tgt) %>% 
    dplyr::group_by(source) %>% 
    unique() %>% 
    dplyr::summarise(
      start = min(created_at),
      end = max(created_at)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::inner_join(gt[["nodes"]], by = c("source" = "nodes"))
  
  if(nrow(nodes) != nrow(gt[["nodes"]]))
    warning("incorrect number of nodes", call. = FALSE)

  construct(gt[["tweets"]], gt[["edges"]], nodes)
}

#' Save
#' 
#' Save the graph to file.
#' 
#' @inheritParams gt_collect
#' @param file File name including extension (\code{format}).
#' @param format Format file format, see \link[igraph]{write_graph}.
#' @param ... Any other argument to pass to \link[igraph]{write_graph}.
#' 
#' @examples 
#' \dontrun{
#' # simulate dataset
#' tweets <- data.frame(
#'   text = c("I tweet @you about @him", 
#'            "I tweet @me about @you"),
#'   screen_name = c("me", "him"),
#'   retweet_count = c(19, 5),
#'   created_at = c(Sys.time(), Sys.time() + 15000),
#'   status_id = c(1, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' tweets %>% 
#'   gt_edges(text, screen_name, "created_at") %>% 
#'   gt_nodes(TRUE) %>% 
#'   gt_dyn() %>% 
#'   gt_save()
#' }
#' 
#' @export
gt_save <- function(gt, file = "graphTweets.graphml", format = "graphml", ...){
  gt_graph(gt) %>% 
    igraph::write_graph(., file = file, format = format, ...)
}