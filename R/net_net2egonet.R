#' Make an tidygraph from an edgelist and a set of nodes attributes.
#' @param graph Tidygraph.
#' @param terms Character. Regular expression to find the focal patterns.
#' @return A tidygraph
#' @importFrom igraph graph_from_edgelist
#' @importFrom tidygraph as_tbl_graph
#' @importFrom tidygraph activate
#' @importFrom tidygraph node_distance_to
#' @importFrom tidygraph centrality_degree
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom tibble rownames_to_column
#' @export

net_net2egonet <- function(graph, terms){
  
  name <- NULL
  edges <- NULL
  from <- NULL
  to <- NULL
  distance <- NULL
  degree <- NULL
  
  focal <- as.data.frame(dplyr::filter(tidygraph::activate(graph, "terms"), stringr::str_detect(name, terms)))$id
  
  graph %>%
    tidygraph::activate(edges) %>%
    dplyr::filter(count >= 5, from != to) %>%
    tidygraph::activate("nodes") %>%
    tidygraph::mutate(distance = tidygraph::node_distance_to(focal, mode = "all")) %>%
    dplyr::filter(id %in% focal | distance <= 1) %>%
    tidygraph::mutate(degree = tidygraph::centrality_degree(mode = "all")) %>%
    dplyr::filter(degree > 0)
}
