utils::globalVariables(c("start", "screen_name"))

#' Edges
#' 
#' Get edges from data.frame of tweets.
#' 
#' @param data Data.frame of tweets, usually returned by the \code{rtweet} package.
#' @param tweets Column containing tweets.
#' @param source Author of tweets.
#' @param target Edges target,
#' @param id tweets unique id.
#' @param col,hashtags Column containing co-mentions.
#' @param tl Set to \code{TRUE} to convert hashtags to lower case.
#' @param ... any other column name, see examples.
#' 
#' @section Functions:
#' \itemize{
#'   \item{\code{gt_edges} - Build networks of users.}
#'   \item{\code{gt_co_edges} - Build networks of users to hashtags.}
#' }
#' 
#' @details The \code{tl} arguments stands for \code{\link{tolower}} and allows converting the #hashtags to lower case as 
#' these often duplicated, i.e.: #python #Python.
#' 
#' @examples 
#' # simulate dataset
#' tweets <- data.frame(
#'   text = c("I tweet @you about @him and @her", 
#'            "I tweet @me about @you"),
#'   screen_name = c("me", "him"),
#'   retweet_count = c(19, 5),
#'   status_id = c(1, 2),
#'   hashtags = c("rstats", "Python"),
#'   stringsAsFactors = FALSE
#' )
#'
#' tweets %>% 
#'   gt_edges_from_text(status_id, screen_name, text)
#'     
#' @return An object of class \code{graphTweets}.
#' 
#' @rdname edges
#' @export
gt_edges_from_text <- function(data, id, source, tweets, ...){
  if(missing(data) || missing(tweets) || missing(source) || missing(id))
    stop("missing data, tweets, source, or id", call. = FALSE)
  tweets <- dplyr::enquo(tweets)
  source <- dplyr::enquo(source)
  id <- deparse(substitute(id))
  gt_edges_from_text_(data, id, source, tweets, ...)
}

#' @rdname edges
#' @export
gt_edges_from_text_ <- function(data, id = "status_id", source = "screen_name", tweets = "text", ...){
  
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
    tidyr::separate_rows_("handles") %>% 
    dplyr::mutate(handles = as.character(handles)) %>% 
    dplyr::select_(
      source = source, 
      target = "handles",
      ...
      ) %>% 
    dplyr::as_tibble() %>% 
    dplyr::filter(
      source != "",
      target != ""
    ) -> edges
  
  construct(data, edges, NULL)
  
}

#' @rdname edges
#' @export
gt_edges <- function(data, source, target, ..., tl = TRUE){
  
  if(missing(data) || missing(target) || missing(source))
    stop("missing data, target, or source", call. = FALSE)
  
  col_name <- deparse(substitute(target))
  
  target <- dplyr::enquo(target)
  source <- dplyr::enquo(source)
  
  edges <- data %>% 
    dplyr::select(source = !!source, target = !!target, ...) %>% 
    tidyr::unnest(target) %>% 
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
    dplyr::filter(!is.na(target)) %>% 
    dplyr::mutate(
      target = dplyr::case_when(
        col_name == "hashtags" ~ paste0("#", target),
        TRUE ~ target
      )
    )
  
  construct(data, edges, NULL)
}

#' @rdname edges
#' @export
gt_edges_ <- function(data, tweets = "text", source = "screen_name", id = "status_id", ...){
  
  .Deprecated("gt_edges")
  
  if(missing(data))
    stop("missing data", call. = FALSE)
  
  if(!inherits(data, "data.frame"))
    stop("data is not of class data.frame")
  
  ids <- data[[id]]
  
  if(length(unique(ids)) != nrow(data))
    stop("id are not unique", call. = FALSE)
  
  handles <- data %>%
    dplyr::select(tweets) %>% 
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
    tidyr::separate_rows_("handles") %>% 
    dplyr::mutate(handles = as.character(handles)) %>% 
    dplyr::select_(
      source = source, 
      target = "handles",
      ...
    ) %>% 
    dplyr::as_tibble() %>% 
    dplyr::filter(
      source != "",
      target != ""
    ) -> edges
  
  construct(data, edges, NULL)
  
}

#' @rdname edges
#' @export
gt_co_edges <- function(data, col, tl = TRUE){
  
  if(missing(data))
    stop("missing data or col", call. = FALSE)
  
  bind_edges <- function(x){
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
  
  col_name <- deparse(substitute(col))
  
  edges <- purrr::map(data[[col_name]], bind_edges) %>% 
    purrr::map_df(dplyr::bind_rows) %>% 
    dplyr::mutate(
      source = dplyr::case_when(
        tl == TRUE ~ tolower(source),
        TRUE ~ source
      ),
      target = dplyr::case_when(
        tl == TRUE ~ tolower(target),
        TRUE ~ target
      )
    ) %>% 
    dplyr::group_by(source, target) %>% 
    dplyr::summarise(n_comentions = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(!is.na(target))
  
  construct(data, edges, NULL)
}

#' @rdname edges
#' @export
gt_edges_hashes <- function(data, hashtags,  tl = TRUE){
  
  .Deprecated("gt_co_edges")
  
  if(missing(data) || missing(hashtags))
    stop("missing data or hashtags", call. = FALSE)
  hashtags <- deparse(substitute(hashtags))
  gt_edges_hashes_(data, hashtags, tl = tl)
}

#' @rdname edges
#' @export
gt_edges_hashes_ <- function(data, hashtags = "hashtags", tl = TRUE){
  
  .Deprecated("gt_co_edges")
  
  if(missing(data))
    stop("missing data", call. = FALSE)
  
  b_edges <- function(x){
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
  
  edges <- purrr::map(data[[hashtags]], b_edges) %>% 
    purrr::map_df(dplyr::bind_rows) %>% 
    dplyr::mutate(
      source = dplyr::case_when(
        tl == TRUE ~ tolower(source),
        TRUE ~ source
      ),
      target = dplyr::case_when(
        tl == TRUE ~ tolower(target),
        TRUE ~ target
      )
    ) %>% 
    dplyr::group_by_("source", "target") %>% 
    dplyr::summarise(n_comentions = n()) %>% 
    dplyr::ungroup()
  
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
#'   gt_edges_from_text(status_id, screen_name, text) %>% 
#'   gt_nodes()
#'   
#' @return An object of class \code{graphTweets}, adds \code{nodes}.
#' 
#' @export
gt_nodes <- function(gt, meta = FALSE){
  
  if(missing(gt))
    stop("missing gt", call. = FALSE)
  
  test_input(gt)
  
  nodes <- c(gt[["edges"]][["source"]], gt[["edges"]][["target"]])
  
  if(max(grepl("#", gt$edges$target)) >= 1)
    type <- c(
      rep("user", nrow(gt[["edges"]])),
      rep("hashtag", nrow(gt[["edges"]]))
    )
  else if("n_comentions" %in% names(gt[["edges"]]))
    type <- "hashtag"
  else
    type <- "user"
  
  nodes <- dplyr::tibble(
    nodes = nodes,
    type = type
  ) %>% 
    dplyr::group_by(nodes, type) %>% 
    dplyr::summarise(
      n_edges = n()
    ) %>% 
    dplyr::ungroup()
  
  if(isTRUE(meta)){
    usr <- rtweet::users_data(gt$tweets) %>% dplyr::mutate(screen_name = tolower(screen_name))
    
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
#' Build \code{igraph} object.
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
#'   text = c("I tweet @you about @him and @her", 
#'            "I tweet @me about @you"),
#'   screen_name = c("me", "him"),
#'   created_at = c(Sys.time(), Sys.time() + 10000),
#'   status_id = c(1, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' tweets %>% 
#'   gt_edges(text, screen_name, status_id, "created_at") %>% 
#'   gt_nodes() %>% 
#'   gt_dyn() %>% 
#'   gt_collect() -> net
#' }
#' 
#' @rdname dyn
#' @export
gt_dyn <- function(gt, lifetime = Inf){
  
  test_input(gt)
  
  if(!"created_at" %in% names(gt[["edges"]]))
    stop("missing created_at column", call. = FALSE)
  
  edges <- gt[["edges"]]
  
  if(is.infinite(lifetime)){
    lifetime <- max(edges[["created_at"]])
    edges[["end"]] <- lifetime
  } else {
    edges[["end"]] <- edges[["created_at"]] + lifetime 
  }
  
  src <- edges[, c("source", "created_at")]
  src2 <- edges[, c("source", "end")]
  tgt <- edges[, c("target", "created_at")]
  tgt2 <- edges[, c("target", "end")]
  
  rename <- function(x){
    names(x)[1:2] <- c("source", "created_at")
    return(x)
  }
  
  src2 <- rename(src2)
  tgt <- rename(tgt)
  tgt2 <- rename(tgt2)
  
  nodes <- src %>% 
    dplyr::bind_rows(tgt, tgt2, src2) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(source) %>% 
    dplyr::summarise(
      start = min(created_at),
      end = max(created_at)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct() %>% 
    dplyr::inner_join(gt[["nodes"]], by = c("source" = "nodes"))
  
  names(nodes)[1] <- "nodes"
  
  if(nrow(nodes) != nrow(gt[["nodes"]]))
    warning("incorrect number of nodes", call. = FALSE)

  construct(gt[["tweets"]], edges, nodes)
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
