#' \pkg{graphTweets} visualise Twitter Interactions.
#'  
#' @section Functions:
#' \itemize{
#' \item \code{\link{gt_edges}} \code{\link{gt_edges_}} - get edges from tweets
#' \item \code{\link{gt_nodes}} - add meta-data to vertices
#' \item \code{\link{gt_edges_hash}} - hashtags networks
#' \item \code{\link{gt_dyn}} - create dynamic graphs
#' \item \code{\link{gt_collect}} - collect edges and nodes
#' \item \code{\link{gt_graph}} - create graph
#' \item \code{\link{gt_save}} - save graph to file
#' }
#' 
#' @keywords internal
#' 
#' @examples 
#' \dontrun{
#' library(rtweet)
#' 
#' # Sys.setlocale("LC_TIME", "English")
#' 
#' tweets <- search_tweets("#rstats")
#' 
#' library(graphTweets)
#' 
#' # simple network
#' tweets %>% 
#'   gt_edges(text, screen_name, status_id) %>% # get edges
#'   gt_nodes %>% # get nodes
#'   gt_graph %>% # build igraph object
#'   plot(.)
#'   
#' # dynamic graph
#' tweets %>% 
#'   gt_edges(text, screen_name, status_id, "created_at") %>% # add created time
#'   gt_nodes(TRUE) %>% 
#'   gt_graph %>% 
#'   gt_dyn %>% # make dynamic
#'   gt_save # save as .graphml
#' }
#' 
#' @importFrom dplyr n
#' @name graphTweets
#' @docType package
NULL