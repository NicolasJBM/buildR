#' Make an tidygraph from an edgelist and a set of nodes attributes.
#' @param x                Tibble. Edge list; the two first variables have to be "from" and "to", the subsequent ones are treated as edge properties.
#' @param nodes_properties Tibble. Properties of the nodes. The first variable has to be called "name" and contain the node id/label. Subsequent variables are treated as node properties.
#' @return A tidygraph
#' @importFrom igraph graph_from_edgelist
#' @importFrom tidygraph as_tbl_graph
#' @importFrom tidygraph activate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
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
  
  # Create the graph from the list od edges
  graph <- x %>%
    select(from, to) %>%
    as.matrix() %>%
    graph_from_edgelist(directed = TRUE) %>%
    as_tbl_graph()
  
  # Change from names to ids to append edge attributes
  nodes <- graph %>%
    activate(nodes) %>%
    as.data.frame() %>%
    rownames_to_column("id") %>%
    select(id, from = name) %>%
    mutate(to = from) %>%
    mutate(id = as.integer(id))
  
  edge_properties <- x %>%
    left_join(select(nodes, from, id), by = "from") %>%
    select(-from) %>%
    rename(from = id) %>%
    left_join(select(nodes, to, id), by = "to") %>%
    select(-to) %>%
    rename(to = id) %>%
    select(from, to, everything())
  
  # Append nodes and edges attributes
  if (!is.null(nodes_properties)){
    graph <-graph %>%
      activate(nodes) %>%
      left_join(nodes_properties, by = "name")
  }
  
  if (length(x) > 2){
    graph <-graph %>%
      activate(edges) %>%
      left_join(edge_properties, by = c("from","to"))
  }
  
  # Return the results
  return(graph)
  
}