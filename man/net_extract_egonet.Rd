% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/net_extract_egonet.R
\name{net_extract_egonet}
\alias{net_extract_egonet}
\title{Extract an ego-network}
\usage{
net_extract_egonet(graph, pattern, separation = 2)
}
\arguments{
\item{graph}{Tidygraph.}

\item{pattern}{Character. Regular expression to find the focal patterns.}

\item{separation}{Numeric. Maximum number of edges between focal and selected nodes.}
}
\value{
A tidygraph
}
\description{
Extract an ego-network around a pattern
}
\author{
Nicolas Mangin
}
