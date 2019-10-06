#' Make an tidygraph from an edgelist and a set of nodes attributes.
#' @param x                Tibble. Edge list; the two first variables have to be "from" and "to", the subsequent ones are treated as edge properties.
#' @param nodes_properties Tibble. Properties of the nodes. The first variable has to be called "name" and contain the node id/label. Subsequent variables are treated as node properties.
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


net_edge2graph <- function(x, nodes_properties = NULL){
  
  # check entries
  stopifnot(
    !is.null(x),
    names(x)[1] == "from",
    names(x)[2] == "to",
    is.null(nodes_properties) | names(nodes_properties)[1] == "name"
  )
  
  # Bind variables
  from <- NULL
  to <- NULL
  name <- NULL
  edges <- NULL
  id <- NULL
  from_rank <- NULL
  to_rank <- NULL
  
  # Create the graph from the list od edges
  graph <- x %>%
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
  edges <- x %>%
    dplyr::left_join(dplyr::select(nodes, from = name, from_rank = rank),  by = "from") %>%
    dplyr::left_join(dplyr::select(nodes, to = name, to_rank = rank),  by = "to") %>%
    dplyr::select(-from, -to) %>%
    dplyr::select(from = from_rank, to = to_rank, dplyr::everything())
  
  # Append nodes and edges attributes
  if (!is.null(nodes_properties)){
    nodes <- nodes %>%
      dplyr::left_join(nodes_properties, by = "name")
  }
  
  graph <- graph %>%
    tidygraph::activate("nodes") %>%
    dplyr::full_join(nodes, by = "name") %>%
    tidygraph::activate("edges") %>%
    dplyr::full_join(edges, by = c("from","to"))
  
  # Return the results
  return(graph)
  
}