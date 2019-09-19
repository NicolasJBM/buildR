#' Make an edgelist from a set of nodes attributes.
#' @param x     Tibble or dataframe. Table with one variable indicating the nodes.
#' @param nodes Character sting. Indicate the name of the variable used as node id.
#' @return A tibble with, for each pair of nodes, the distance (for numeric variables) or cooccurrences (categorical variables).
#' @importFrom tidyr gather
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tidyr replace_na
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise_all
#' @importFrom dplyr mutate_if
#' @importFrom purrr reduce
#' @importFrom dplyr full_join
#' @importFrom purrr map
#' @importFrom gtools permutations
#' @importFrom tibble as_tibble
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom stats dist
#' @export


tble_attri2edge <- function(x, nodes = NULL){
  
  # Check entries
  stopifnot(!is.null(nodes))
  
  # Bind variables
  from <- NULL
  to <- NULL
  value <- NULL
  edges <- NULL
  V1 = NULL
  V2 = NULL
  data <- NULL
  category <- NULL
  
  
  # Rename the node variable and identify the others
  x <- x %>%
    select(nodes = nodes, everything()) %>%
    as.data.frame()
  
  variables <- setdiff(names(x), "nodes")
  
  
  # Prepare fsub-functions
  make_dist <- function(x) {
    x %>%
      as.data.frame() %>%
      column_to_rownames("nodes") %>%
      dist() %>%
      as.matrix() %>%
      as.data.frame() %>%
      rownames_to_column("from") %>%
      gather(to, value, -from) %>%
      as_tibble() %>%
      select(from, to, value)
  }
  
  make_cooc <- function(x) {
    x <- x$nodes %>%
      unlist() %>%
      as.character() %>%
      unique()
    
    x <- permutations(length(x),2,x,repeats.allowed=TRUE) %>%
      as.data.frame() %>%
      rename(from = V1, to = V2) %>%
      mutate(from = as.character(from), to = as.character(to)) %>%
      mutate(value = 1) %>%
      select(from, to, value)
  }
  
  
  # Make a list with as many entries as variables and compute distances or cooccurrences
  dist_proxim <- vector("list", length(variables))
  
  for (i in 1:length(variables)){
    tmp <- select(x, nodes, edges = variables[i])
    if (is.numeric(tmp[,2])){
      tmp <- tmp %>%
        group_by(nodes) %>%
        summarise_all(mean) %>%
        ungroup() %>%
        make_dist()
      names(tmp) <- c("from","to",paste0("D_",variables[i]))
    } else {
      tmp <- tmp %>%
        group_by(edges) %>%
        nest() %>%
        mutate(data = map(data, make_cooc)) %>%
        unnest(data) %>%
        ungroup() %>%
        select(-edges) %>%
        group_by(from, to) %>%
        summarise_all(sum) %>%
        ungroup() %>%
        select(from, to, value)
      names(tmp) <- c("from","to",paste0("P_",variables[i]))
    }
    dist_proxim[[i]] <- tmp
  }
  
  edgelist <- reduce(dist_proxim, full_join, by = c("from","to"))
  edgelist <- edgelist %>%
    mutate_if(startsWith(names(edgelist),"P_"), function(x) replace_na(unlist(x), 0))
  
  return(edgelist)
  
}
