#' @name net_create_graph
#' @title Create a graph from an edgelist
#' @author Nicolas Mangin
#' @description Make an tidygraph from an edgelist and a set of node attributes
#' @param edgelist        Tibble. Edge list; the two first variables have to be "from" and "to", the subsequent ones are treated as edge properties.
#' @param node_attributes Tibble. Properties of the nodes. The first variable has to be called "name" and contain the node id/label. Subsequent variables are treated as node properties.
#' @return A tidygraph
#' @importFrom igraph graph_from_edgelist
#' @importFrom tidygraph as_tbl_graph
#' @importFrom tidygraph activate
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom dplyr left_join
#' @importFrom dplyr full_join
#' @importFrom dplyr rename
#' @importFrom tibble rownames_to_column
#' @export


net_create_graph <- function(edgelist, node_attributes = NULL) {

  # check entries
  stopifnot(
    !is.null(edgelist),
    names(edgelist)[1] == "from",
    names(edgelist)[2] == "to",
    is.null(node_attributes) | names(node_attributes)[1] == "name"
  )

  # Bind variables
  from <- NULL
  to <- NULL
  name <- NULL
  edges <- NULL
  from_rank <- NULL
  to_rank <- NULL

  # Create the graph from the list od edges
  graph <- edgelist %>%
    dplyr::select(from, to) %>%
    as.matrix() %>%
    igraph::graph_from_edgelist(directed = TRUE) %>%
    tidygraph::as_tbl_graph()

  # Change from names to ids to append edge attributes
  nodes <- graph %>%
    tidygraph::activate("nodes") %>%
    as.data.frame() %>%
    tibble::rownames_to_column("rank") %>%
    dplyr::mutate(rank = as.numeric(rank))

  # Replace name by rank in edges
  edges <- edgelist %>%
    dplyr::left_join(dplyr::select(
      nodes,
      from = name, from_rank = rank
    ), by = "from") %>%
    dplyr::left_join(dplyr::select(
      nodes,
      to = name, to_rank = rank
    ), by = "to") %>%
    dplyr::select(-from, -to) %>%
    dplyr::select(from = from_rank, to = to_rank, dplyr::everything())

  # Append nodes and edges attributes
  if (!is.null(node_attributes)) {
    nodes <- nodes %>%
      dplyr::left_join(node_attributes, by = "name")
  }

  graph <- graph %>%
    tidygraph::activate("nodes") %>%
    dplyr::full_join(nodes, by = "name") %>%
    tidygraph::activate("edges") %>%
    dplyr::full_join(edges, by = c("from", "to"))

  # Return the results
  return(graph)
}
