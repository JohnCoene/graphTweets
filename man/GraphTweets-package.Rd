\name{GraphTweets}
\alias{GraphTweets}
\alias{GraphTweets}
\docType{package}
\title{
GraphTweets
}
\description{
Builds unconnected graph from data frame of tweet, can be visualised with \href{http://cran.r-project.org/web/packages/igraph/igraph.pdf}{igraph} package.
}
\details{
\tabular{ll}{
Package: \tab GraphTweets\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-09-30\cr
License: \tab MIT\cr
}
see \href{socialfunction.wordpress.com}{SocialFunction}
}
\author{
John Coene <jcoenep@gmail.com>
}
\keyword{ package }
\seealso{
The \href{http://cran.r-project.org/web/packages/streamR/streamR.pdf}{streamR}, and
\href{https://github.com/geoffjentry/twitteR}{twitteR} both allow obtaining necessary data.

Plot graph using;
\href{http://cran.r-project.org/web/packages/igraph/igraph.pdf}{igraph}

Examples;
\href{http://socialfunction.wordpress.com/}{SocialFunction}
}
\examples{
\dontrun{

#Build Edge table
edges <- edge_table(tweets, "text", "user_ID", "longitude", "latitude", "gender")

#Get Meta-data
nodes <- node_table(edges, "longitude", "latitude", "gender")

#Build and plot graph
library(igraph)
graph <- graph.data.frame(edges, directed=TRUE, vertices=nodes)
plot(graph)
}
}
