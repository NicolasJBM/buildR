#' @name net_compute_attributes
#' @title Compute network attributes for edges and nodes
#' @author Nicolas Mangin
#' @description Compute and append network attributes for both edges and nodes
#' @param graph        Tidygraph.
#' @param directed     Logical. Whether the graph id directed.
#' @param weight_var   Character. Name of the variable containing weights to be used
#' @param optimal_com  Logical. Whether the optimal algorithm should be used to find communities.
#' @return A tidygraph with network metrics for both edges and nodes.
#' @importFrom tidygraph activate
#' @importFrom tidygraph edge_is_mutual
#' @importFrom tidygraph centrality_edge_betweenness
#' @importFrom tidygraph to_directed
#' @importFrom tidygraph centrality_degree
#' @importFrom tidygraph centrality_betweenness
#' @importFrom tidygraph centrality_authority
#' @importFrom tidygraph centrality_eigen
#' @importFrom tidygraph centrality_hub
#' @importFrom tidygraph centrality_integration
#' @importFrom tidygraph centrality_communicability
#' @importFrom tidygraph group_walktrap
#' @importFrom tidygraph centrality_closeness
#' @importFrom tidygraph morph
#' @importFrom tidygraph unmorph
#' @importFrom tidygraph group_optimal
#' @importFrom tidygraph centrality_alpha
#' @importFrom tidygraph group_infomap
#' @importFrom tidygraph to_undirected
#' @importFrom tidygraph to_subgraph
#' @importFrom dplyr mutate
#' @export

net_compute_attributes <- function(
                                   graph,
                                   directed = TRUE,
                                   weight_var = NULL,
                                   optimal_com = FALSE) {
  component <- NULL

  graph <- graph %>%
    tidygraph::activate("edges") %>%
    dplyr::mutate(
      reciprocity = tidygraph::edge_is_mutual(),
      betweenness = tidygraph::centrality_edge_betweenness()
    ) %>%
    tidygraph::activate("nodes")

  if (directed) {
    graph <- graph %>%
      tidygraph::to_directed() %>%
      dplyr::mutate(
        component = tidygraph::group_components(type = "strong"),
        cent_degree = tidygraph::centrality_degree(mode = "all"),
        cent_indegree = tidygraph::centrality_degree(mode = "in"),
        cent_outdegree = tidygraph::centrality_degree(mode = "out"),
        cent_betweenness = tidygraph::centrality_betweenness(),
        cent_authority = tidygraph::centrality_authority(),
        cent_eigen = tidygraph::centrality_eigen(),
        cent_hub = tidygraph::centrality_hub(),
        cent_integration = tidygraph::centrality_integration(),
        cent_communicability = tidygraph::centrality_communicability(),
        com_walktrap = as.factor(tidygraph::group_walktrap())
      ) %>%
      tidygraph::morph(to_subgraph, subset_by = "nodes", component == 1) %>%
      dplyr::mutate(cent_closeness = tidygraph::centrality_closeness()) %>%
      tidygraph::unmorph()

    if (optimal_com) {
      graph <- graph %>%
        dplyr::mutate(com_optimal = as.factor(tidygraph::group_optimal()))
    }

    if (!is.null(weight_var)) {
      graph <- graph %>%
        dplyr::mutate(
          cent_betweenness_w = tidygraph::centrality_betweenness(weights = get(weight_var)),
          cent_alpha_w = tidygraph::centrality_alpha(weights = get(weight_var)),
          cent_authority_w = tidygraph::centrality_authority(weights = get(weight_var)),
          cent_eigen_w = tidygraph::centrality_eigen(weights = get(weight_var)),
          cent_hub_w = tidygraph::centrality_hub(weights = get(weight_var)),
          com_infomap_w = as.factor(tidygraph::group_infomap(weights = get(weight_var))),
          com_walktrap_w = as.factor(tidygraph::group_walktrap(weights = get(weight_var)))
        ) %>%
        tidygraph::morph(to_subgraph, subset_by = "nodes", component == 1) %>%
        dplyr::mutate(cent_closeness_w = tidygraph::centrality_closeness(weights = get(weight_var))) %>%
        tidygraph::unmorph()

      if (optimal_com) {
        graph <- graph %>%
          dplyr::mutate(com_optimal_w = as.factor(tidygraph::group_optimal(weights = get(weight_var))))
      }
    }
  } else {
    graph <- graph %>%
      tidygraph::to_undirected() %>%
      dplyr::mutate(
        component = tidygraph::group_components(type = "strong"),
        cent_degree = tidygraph::centrality_degree(mode = "all"),
        cent_betweenness = tidygraph::centrality_betweenness(),
        cent_alpha = tidygraph::centrality_alpha(),
        cent_authority = tidygraph::centrality_authority(),
        cent_power = tidygraph::centrality_power(),
        cent_eigen = tidygraph::centrality_eigen(),
        cent_hub = tidygraph::centrality_hub(),
        cent_integration = tidygraph::centrality_integration(),
        cent_communicability = tidygraph::centrality_communicability(),
        com_walktrap = as.factor(tidygraph::group_walktrap()),
        com_louvain = as.factor(tidygraph::group_louvain()),
        com_edge_betweenness = as.factor(tidygraph::group_edge_betweenness(directed = directed))
      ) %>%
      tidygraph::morph(to_subgraph, subset_by = "nodes", component == 1) %>%
      dplyr::mutate(cent_closeness = tidygraph::centrality_closeness()) %>%
      tidygraph::unmorph()

    if (optimal_com) {
      graph <- graph %>%
        dplyr::mutate(com_optimal = as.factor(tidygraph::group_optimal()))
    }

    if (!is.null(weight_var)) {
      graph <- graph %>%
        dplyr::mutate(
          cent_betweenness_w = tidygraph::centrality_betweenness(weights = get(weight_var)),
          cent_alpha_w = tidygraph::centrality_alpha(weights = get(weight_var)),
          cent_authority_w = tidygraph::centrality_authority(weights = get(weight_var)),
          cent_eigen_w = tidygraph::centrality_eigen(weights = get(weight_var)),
          cent_hub_w = tidygraph::centrality_hub(weights = get(weight_var)),
          com_walktrap_w = as.factor(tidygraph::group_walktrap(weights = weight_var))
        ) %>%
        tidygraph::morph(to_subgraph, subset_by = "nodes", component == 1) %>%
        dplyr::mutate(cent_closeness_w = tidygraph::centrality_closeness(weights = get(weight_var))) %>%
        tidygraph::unmorph()

      if (optimal_com) {
        graph <- graph %>%
          dplyr::mutate(com_optimal_w = as.factor(tidygraph::group_optimal(weights = get(weight_var))))
      }
    }
  }

  return(graph)
}
