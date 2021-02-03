#' @name net_compute_structure
#' @title Compute structural metrics for a network
#' @author Nicolas Mangin
#' @description Compute metrics describing the structure of a network
#' @param graph    Tidygraph.
#' @param subgroup Character. Name of the variable containing the community of the nodes in each graph. 
#' @return A tibble with network structure metrics for the graph.
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr summarise
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom tidygraph activate
#' @importFrom tidygraph morph
#' @importFrom tidygraph unmorph
#' @importFrom tidygraph graph_adhesion
#' @importFrom tidygraph graph_assortativity
#' @importFrom tidygraph graph_automorphisms
#' @importFrom tidygraph graph_clique_num
#' @importFrom tidygraph graph_clique_count
#' @importFrom tidygraph graph_motif_count
#' @importFrom tidygraph graph_diameter
#' @importFrom tidygraph graph_radius
#' @importFrom tidygraph graph_mutual_count
#' @importFrom tidygraph graph_asym_count
#' @importFrom tidygraph graph_reciprocity
#' @importFrom tidygraph graph_size
#' @importFrom tidygraph graph_order
#' @importFrom tidygraph graph_min_cut
#' @importFrom tidygraph graph_mean_dist
#' @importFrom tidygraph to_subgraph
#' @importFrom tidygraph to_split
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


net_compute_structure <- function(graph, subgroup){
  
  component <- NULL
  community <- NULL
  graph_possible_edges <- NULL
  
  g <- graph %>%
    tidygraph::activate("nodes") %>%
    dplyr::filter(component == 1) %>%
    igraph::as.igraph()
  
  dyads <- igraph::dyad_census(g)
  triads <- igraph::triad_census(g)
  
  structure <- graph %>%
    tidygraph::activate("nodes") %>%
    dplyr::mutate(
      community = get(subgroup),
      graph_components = max(component),
      graph_order = tidygraph::graph_order()
    ) %>%
    tidygraph::morph(to_subgraph, subset_by = "nodes", component == 1) %>%
    dplyr::mutate(
      graph_adhesion = tidygraph::graph_adhesion(),
      graph_assortativity = tidygraph::graph_assortativity(community),
      graph_automorphisms = tidygraph::graph_automorphisms(),
      graph_largest_clique = tidygraph::graph_clique_num(),
      graph_clique_count = tidygraph::graph_clique_count(min = 3),
      graph_motif = tidygraph::graph_motif_count(),
      graph_diameter = tidygraph::graph_diameter(),
      graph_radius = tidygraph::graph_radius(),
      graph_mutual = tidygraph::graph_mutual_count(),
      graph_asym = tidygraph::graph_asym_count(),
      graph_reciprocity = tidygraph::graph_reciprocity(),
      graph_size = tidygraph::graph_size(),
      graph_component_order = tidygraph::graph_order(),
      graph_min_cut = tidygraph::graph_min_cut(),
      graph_mean_dist = tidygraph::graph_mean_dist()
    ) %>%
    tidygraph::unmorph() %>%
    dplyr::mutate(
      graph_possible_edges = length(igraph::V(g)) * (length(igraph::V(g))-1),
      graph_triads_potential = triads[[1]],
      graph_triads_cut = triads[[2]] + triads[[3]],
      graph_triads_line =
        triads[[4]] + triads[[5]] + triads[[6]] +
        triads[[7]] + triads[[8]] + triads[[11]],
      graph_triads_partial =
        triads[[15]] + triads[[14]] + triads[[13]] +
        triads[[12]] + triads[[9]] + triads[[10]],
      graph_triads_full = triads[[16]],
      graph_density_ties = (2 * dyads$mut + dyads$asym)/ graph_possible_edges,
      graph_density_pairs = (dyads$mut*2 + dyads$asym) / graph_size,
      graph_central_between =
        igraph::centralization.betweenness(g)$centralization,
      graph_central_degree =
        igraph::centralization.degree(g)$centralization,
      graph_transitivity =
        igraph::transitivity(g),
      graph_reciprocity =
        igraph::reciprocity(g, mode = "default"),
      graph_reciprocity_ratio =
        igraph::reciprocity(g, mode = "ratio")
    ) %>%
    tidygraph::morph(to_split, community, split_by = "nodes") %>%
    dplyr::mutate(
      com_adhesion = tidygraph::graph_adhesion(),
      com_automorphisms = tidygraph::graph_automorphisms(),
      com_largest_clique = tidygraph::graph_clique_num(),
      com_clique_count = tidygraph::graph_clique_count(min = 3),
      com_motif = tidygraph::graph_motif_count(),
      com_diameter = tidygraph::graph_diameter(),
      com_radius = tidygraph::graph_radius(),
      com_mutual = tidygraph::graph_mutual_count(),
      com_asym = tidygraph::graph_asym_count(),
      com_reciprocity = tidygraph::graph_reciprocity(),
      com_size = tidygraph::graph_size(),
      com_component_order = tidygraph::graph_order(),
      com_min_cut = tidygraph::graph_min_cut(),
      com_mean_dist = tidygraph::graph_mean_dist()
    ) %>%
    tidygraph::unmorph()
  
  return(structure)
}
