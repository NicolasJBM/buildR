#' Add Jaccard coefficients to the edges of a tidygraph.
#' @param graph Tidygraph.
#' @return A tidygraph with Jaccard coefficients.
#' @importFrom tidygraph activate
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @export


net_compute_overlap <- function(graph){
  
  edges <- NULL
  from <- NULL
  to <- NULL
  weight <- NULL
  
  occ_from <- graph %>%
    tidygraph::activate("edges") %>%
    as.data.frame() %>%
    dplyr::filter(from == to) %>%
    dplyr::select(from, occ_from = weight)
  
  occ_to <- occ_from %>%
    dplyr::rename(to = from, occ_to = occ_from)
  
  graph %>%
    tidygraph::activate("edges") %>%
    dplyr::left_join(occ_from, by = "from") %>%
    dplyr::left_join(occ_to, by = "to") %>%
    dplyr::mutate(
      jaccard = weight / (occ_from + occ_to - weight),
      from_if_to = weight / occ_to,
      to_if_from = weight / occ_from
    ) %>%
    dplyr::select(-occ_from, -occ_to) %>%
    tidygraph::activate("nodes") %>%
    dplyr::mutate(occurrences = arrange(occ_from, from)$occ_from)
}
