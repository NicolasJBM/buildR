#' @name net_create_edgelist
#' @title Create an edgelist from attributes
#' @author Nicolas Mangin
#' @description Create edges based on distances between attributes or based on common attributes.
#' @param x        Tibble or dataframe. Table with one variable indicating the nodes.
#' @param node_var Character sting. Indicate the name of the variable used as node id.
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


net_create_edgelist <- function(x, node_var = NULL) {

  # Check entries
  stopifnot(!is.null(node_var))

  # Bind variables
  from <- NULL
  to <- NULL
  value <- NULL
  edges <- NULL
  data <- NULL


  # Rename the node variable and identify the others
  x <- x %>%
    dplyr::select(node_var = node_var, dplyr::everything()) %>%
    as.data.frame()

  variables <- setdiff(names(x), "node_var")

  # Make a list with as many entries as variables and
  # compute distances or cooccurrences
  dist_proxim <- vector("list", length(variables))

  for (i in seq_len(length(variables))) {
    tmp <- dplyr::select(x, node_var, edges = variables[i])
    if (is.numeric(tmp[, 2])) {
      tmp <- tmp %>%
        dplyr::group_by(node_var) %>%
        dplyr::summarise_all(mean) %>%
        dplyr::ungroup() %>%
        make_dist()
      names(tmp) <- c("from", "to", paste0("D_", variables[i]))
    } else {
      tmp <- tmp %>%
        dplyr::group_by(edges) %>%
        tidyr::nest() %>%
        dplyr::mutate(data = map(data, make_cooc)) %>%
        tidyr::unnest(data) %>%
        dplyr::ungroup() %>%
        dplyr::select(-edges) %>%
        dplyr::group_by(from, to) %>%
        dplyr::summarise_all(sum) %>%
        dplyr::ungroup() %>%
        dplyr::select(from, to, value)
      names(tmp) <- c("from", "to", paste0("P_", variables[i]))
    }
    dist_proxim[[i]] <- tmp
  }

  edgelist <- purrr::reduce(dist_proxim, full_join, by = c("from", "to"))
  edgelist <- edgelist %>%
    dplyr::mutate_if(
      startsWith(names(edgelist), "P_"),
      function(x) replace_na(unlist(x), 0)
    )

  return(edgelist)
}


# Prepare fsub-functions
make_dist <- function(x) {
  from <- NULL
  to <- NULL
  value <- NULL

  y <- x %>%
    as.data.frame() %>%
    tibble::column_to_rownames("node_var") %>%
    dist() %>%
    as.matrix() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("from")

  y <- y %>%
    tidyr::pivot_longer(
      cols = setdiff(names(y), "from"),
      names_to = "to",
      values_to = "value"
    ) %>%
    tibble::as_tibble() %>%
    dplyr::select(from, to, value)
  y
}


make_cooc <- function(x) {
  from <- NULL
  to <- NULL
  value <- NULL
  V1 <- NULL
  V2 <- NULL
  y <- x$node_var %>%
    unlist() %>%
    as.character() %>%
    unique()
  y <- gtools::permutations(length(y), 2, y, repeats.allowed = TRUE) %>%
    as.data.frame() %>%
    dplyr::rename(from = V1, to = V2) %>%
    dplyr::mutate(from = as.character(from), to = as.character(to)) %>%
    dplyr::mutate(value = 1) %>%
    dplyr::select(from, to, value)
  y
}
