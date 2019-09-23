#' Add Jaccard coefficients to the edges of a tidygraph.
#' @param graph        Tidygraph.
#' @param directed     Logical. Whether the graph id directed.
#' @param weight_var   Character. Name of the variable containing weights to be used
#' @param optimal_com  Logical. Whether the optimal algorithm should be used to find communities.
#' @return A tidygraph with network metrics for both edges and nodes.
#' @import tidygraph
#' @import dplyr
#' @export

net_compute_metrics <- function(graph, directed = TRUE, weight_var = NULL, optimal_com = FALSE){
  
  graph <- graph %>%
    mutate(
      reciprocity = edge_is_mutual(),
      betweenness = centrality_edge_betweenness() 
    ) %>%
    activate("nodes")
  
  if (directed){
    graph <- graph %>%
      to_directed() %>%
      mutate(
        cent_degree = centrality_degree(mode = "all"),
        cent_indegree = centrality_degree(mode = "in"),
        cent_outdegree = centrality_degree(mode = "out"),
        cent_betweenness = centrality_betweenness(),
        cent_authority = centrality_authority(),
        cent_closeness = centrality_closeness(),
        cent_eigen = centrality_eigen(),
        cent_hub = centrality_hub(),
        cent_integration = centrality_integration(),
        cent_communicability = centrality_communicability(),
        com_infomap = as.factor(group_infomap()),
        com_walktrap = as.factor(group_walktrap())
      )
    
    if (optimal_com){
      graph <- graph %>%
        mutate(com_optimal = as.factor(group_optimal()))
    }
    
    if (!is.null(weight_var)){
      graph <- graph  %>%
        mutate(
          cent_betweenness_w = centrality_betweenness(weights = get(weight_var)),
          cent_alpha_w = centrality_alpha(weights = get(weight_var)),
          cent_authority_w = centrality_authority(weights = get(weight_var)),
          cent_closeness_w = centrality_closeness(weights = get(weight_var)),
          cent_eigen_w = centrality_eigen(weights = get(weight_var)),
          cent_hub_w = centrality_hub(weights = get(weight_var)),
          com_infomap_w = as.factor(group_infomap(weights = get(weight_var))),
          com_walktrap_w = as.factor(group_walktrap(weights = get(weight_var)))
        )
      
      if (optimal_com){
        graph <- graph %>%
          mutate(com_optimal_w = as.factor(group_optimal(weights = get(weight_var))))
      }
      
    }
    
  } else {
    graph <- graph %>%
      to_undirected() %>%
      mutate(
        cent_degree = centrality_degree(mode = "all"),
        cent_betweenness = centrality_betweenness(),
        cent_alpha = centrality_alpha(),
        cent_authority = centrality_authority(),
        cent_power = centrality_power(),
        cent_closeness = centrality_closeness(),
        cent_eigen = centrality_eigen(),
        cent_hub = centrality_hub(),
        cent_integration = centrality_integration(),
        cent_communicability = centrality_communicability(),
        com_infomap = as.factor(group_infomap()),
        com_walktrap = as.factor(group_walktrap()),
        com_louvain = as.factor(group_louvain()),
        com_edge_betweenness = as.factor(group_edge_betweenness(directed = directed))
      )
    
    if (optimal_com){
      graph <- graph %>%
        mutate(com_optimal = as.factor(group_optimal()))
    }
    
    if (!is.null(weight_var)){
      graph <- graph  %>%
        mutate(
          cent_betweenness_w = centrality_betweenness(weights = get(weight_var)),
          cent_alpha_w = centrality_alpha(weights = get(weight_var)),
          cent_authority_w = centrality_authority(weights = get(weight_var)),
          cent_closeness_w = centrality_closeness(weights = get(weight_var)),
          cent_eigen_w = centrality_eigen(weights = get(weight_var)),
          cent_hub_w = centrality_hub(weights = get(weight_var)),
          com_walktrap_w = as.factor(group_walktrap(weights = weight_var))
        )
      
      if (optimal_com){
        graph <- graph %>%
          mutate(com_optimal_w = as.factor(group_optimal(weights = get(weight_var))))
      }
    }
  }
  
  return(graph)
}
