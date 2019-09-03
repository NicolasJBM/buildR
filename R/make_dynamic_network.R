#' Create a dynamic network from periodic edgelists after filtering edges based on a weight criterion.
#' @param x dataframe. List of relationships for one group over several periods. It must have at least 4 columns: one for the period, "from" and "to" for the edges, and an attribute.
#' @param period character. Name of the variable indicating the period. 
#' @param weight character. Name of the variable used to weight edges.
#' @param threshold numeric. Minimum value of the weighting attribute for the relationship to be maintained.
#' @param cores integer. Number of cores to be used for parallel computing.
#' @return A dynamic network which can be rendered with render.d3movie
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom tidyr nest
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom networkDynamic networkDynamic
#' @importFrom purrr map
#' @importFrom tibble remove_rownames
#' @importFrom tibble column_to_rownames
#' @importFrom network as.network
#' @importFrom parallel parLapply
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @export

make_dynamic_network <- function(x,
                                 period = "period",
                                 weight = "weight",
                                 threshold = 0.5,
                                 cores = 2){
  
  # Bind variables
  data <- NULL
  target <- NULL
  from <- NULL
  to <- NULL
  
  # Transform the edgelist into a list of network class object
  
  networks <- x %>%
    dplyr::select(period = period, from, to, weight = weight) %>%
    dplyr::filter(weight >= threshold) %>%
    spread(key = to, value = weight, fill = 0) %>%
    gather(to, weight, -period, -from) %>%
    spread(key = from, value = weight, fill = 0) %>%
    gather(from, weight, -period, -to) %>%
    group_by(period) %>%
    nest()
  
  clust <- makeCluster(cores)
  networks$data <- parLapply(cl = clust, X = networks$data, fun = prep_dynet)
  stopCluster(clust)
  
  # Create the dynamic network
  dynet <- networkDynamic(network.list = networks$data)
  
  return(dynet)
}


prep_dynet <- function(x){
  
  to <- NULL
  weight <- NULL
  
  x %>%
    spread(key = to, value = weight, fill = 0) %>%
    as.data.frame() %>%
    remove_rownames() %>%
    column_to_rownames(var = "from") %>%
    as.matrix() %>%
    as.network(matrix.type = "adjacency",
               directed = TRUE,
               ignore.eval=FALSE,
               names.eval='weight')
  
}
