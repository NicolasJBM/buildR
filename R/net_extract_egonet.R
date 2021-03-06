#' @name net_extract_egonet
#' @title Extract an ego-network
#' @author Nicolas Mangin
#' @description Extract an ego-network around a pattern
#' @param graph      Tidygraph.
#' @param pattern    Character. Regular expression to find the focal patterns.
#' @param separation Numeric. Maximum number of edges between focal and selected nodes.
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

net_extract_egonet <- function(graph, pattern, separation = 2) {
  name <- NULL
  distance <- NULL
  degree <- NULL
  label <- NULL

  nodes <- as.data.frame(tidygraph::activate(graph, "nodes")) %>%
    dplyr::mutate(name = seq_len(length(label)))

  focal <- dplyr::filter(nodes, stringr::str_detect(label, pattern))$name

  graph %>%
    tidygraph::activate("nodes") %>%
    tidygraph::mutate(
      distance = tidygraph::node_distance_to(focal, mode = "all")
    ) %>%
    dplyr::filter(name %in% focal | distance <= separation) %>%
    tidygraph::mutate(degree = tidygraph::centrality_degree(mode = "all")) %>%
    dplyr::filter(degree > 0)
}
