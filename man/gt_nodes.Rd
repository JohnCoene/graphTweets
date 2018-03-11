% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nodes.R, R/overhaul.R
\name{gt_nodes}
\alias{gt_nodes}
\alias{gt_nodes}
\title{Nodes}
\usage{
gt_nodes(gt, meta = FALSE)

gt_nodes(gt, meta = FALSE)
}
\arguments{
\item{gt}{An object of class \code{graphTweets} as returned by \code{\link{gt_edges}}.}

\item{meta}{Set to \code{TRUE} to add meta data to nodes.}
}
\value{
An object of class \code{graphTweets}.
}
\description{
Nodes

Get nodes from a \code{graphTweets} object.
}
\examples{
# simulate dataset
tweets <- data.frame(
  text = c("I tweet @you about @him", 
           "I tweet @me about @you"),
  screen_name = c("me", "him"),
  retweet_count = c(19, 5),
  status_id = c(1, 2),
  stringsAsFactors = FALSE
)

tweets \%>\% 
  gt_edges(text, screen_name) \%>\% 
  gt_nodes() -> net
  
}