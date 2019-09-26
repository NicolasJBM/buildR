#' Aggregate edges according to speciffications.
#' @param graph Tidygraph.
#' @param node_com    Character. Name of the variable containing the community of the nodes in each graph. 
#' @return A tibble with network structure metrics for the graph.
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr summarise
#' @importFrom dplyr bind_cols
#' @importFrom tidygraph activate
#' @importFrom igraph as.igraph
#' @importFrom igraph dyad_census
#' @importFrom igraph triad_census
#' @importFrom igraph V
#' @importFrom igraph assortativity_degree
#' @importFrom igraph centralization.betweenness
#' @importFrom igraph centralization.degree
#' @importFrom igraph transitivity
#' @importFrom igraph reciprocity
#' @importFrom igraph gsize
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @export


net_structure_metrics <- function(graph, node_com){
  
  cent_degree <- NULL
  occurrences <- NULL
  possible_edges <- NULL
  actual_edges <- NULL
  actual_edges <- NULL
  name <- NULL
  community <- NULL
  from_com <- NULL
  to_com <- NULL
  
  # remove singletons
  connected <- graph %>%
    tidygraph::activate("nodes") %>%
    dplyr::filter(cent_degree > 0)
  
  # compute metrics
  structure1 <- tibble::tibble(
    missing = nrow(dplyr::filter(as.data.frame(tidygraph::activate(graph, "nodes")), occurrences == 0)),
    singleton = nrow(dplyr::filter(as.data.frame(tidygraph::activate(graph, "nodes")), occurrences > 0, cent_degree == 0)),
    connected = nrow(dplyr::filter(as.data.frame(tidygraph::activate(graph, "nodes")), occurrences > 0, cent_degree > 0)),
  )
  
  g <- igraph::as.igraph(connected)
  dyads <- igraph::dyad_census(connected)
  triads <- igraph::triad_census(g)
  
  structure2 <- tibble::tibble(
    nodes = length(igraph::V(g)),
    possible_edges = length(igraph::V(g)) * (length(igraph::V(g))-1),
    mutual_edges = dyads$mut,
    asymetric_edges = dyads$asym,
    actual_edges = dyads$mut*2 + dyads$asym,
    triads_potential = triads[[1]],
    triads_cut = triads[[2]] + triads[[3]],
    triads_line = triads[[4]] + triads[[5]] + triads[[6]] + triads[[7]] + triads[[8]] + triads[[11]],
    triads_partial = triads[[15]] + triads[[14]] + triads[[13]] + triads[[12]] + triads[[9]] + triads[[10]],
    triads_full = triads[[16]],
    assort_degree = igraph::assortativity_degree(g),
    density_ties = (2 * dyads$mut + dyads$asym)/ possible_edges,
    density_pairs = (dyads$mut*2 + dyads$asym) / actual_edges,
    central_between = igraph::centralization.betweenness(g)$centralization,
    central_degree = igraph::centralization.degree(g)$centralization,
    transitivity = igraph::transitivity(g),
    reciprocity_d = igraph::reciprocity(g, mode = "default"),
    reciprocity_r = igraph::reciprocity(g, mode = "ratio"),
    edge_nbr = igraph::gsize(g)
  )
  
  # Compute communities relationships
  nodes <- connected %>%
    tidygraph::activate("nodes") %>%
    dplyr::select(name, community = node_com) %>%
    as.data.frame() %>%
    dplyr::mutate(name = as.numeric(name))
  
  structure3 <- connected %>%
    tidygraph::activate("edges") %>%
    as.data.frame() %>%
    dplyr::left_join(dplyr::select(nodes, from = name, from_com = community), by = "from") %>%
    dplyr::left_join(dplyr::select(nodes, to = name, to_com = community), by = "to") %>%
    dplyr::select(from_com, to_com) %>%
    dplyr::mutate(within = as.numeric(from_com == to_com), between = as.numeric(from_com != to_com)) %>%
    dplyr::select(within, between) %>%
    na.omit() %>%
    dplyr::summarise(
      within_sum = sum(within),
      within_mean = mean(within),
      within_sd = sd(within),
      between_sum = sum(between),
      between_mean = mean(between),
      between_sd = sd(between)
    )
  
  structure <- structure1 %>%
    dplyr::bind_cols(structure2)%>%
    dplyr::bind_cols(structure3)
  
  return(structure)
}
