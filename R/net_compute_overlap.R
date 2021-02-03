#' @name net_compute_overlap
#' @title Compute overlap metrics
#' @author Nicolas Mangin
#' @description Add Jaccard coefficients and other overlap metrics to the edges of a tidygraph.
#' @param graph Tidygraph.
#' @return A tidygraph with Jaccard coefficients.
#' @importFrom tidygraph activate
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @export


net_compute_overlap <- function(graph) {
  
  from <- NULL
  to <- NULL
  occ_from <- NULL
  weight <- NULL
  occurrences <- NULL

  nodes <- as.data.frame(tidygraph::activate(graph, "nodes"))

  if ("occurrences" %in% names(nodes)) {
    addocc <- nodes %>%
      select(rank, occurrences)
  } else {
    addocc <- graph %>%
      tidygraph::activate("edges") %>%
      as.data.frame() %>%
      dplyr::filter(from == to) %>%
      dplyr::select(rank = from, occurrences = weight)
  }

  occ_from <- addocc %>%
    dplyr::rename(from = rank, occ_from = occurrences)

  occ_to <- addocc %>%
    dplyr::rename(to = rank, occ_to = occurrences)

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
    na.omit() %>%
    tidygraph::activate("nodes") %>%
    dplyr::left_join(addocc, by = "rank") %>%
    tidyr::replace_na(list(occurrences = 0)) %>%
    na.omit()
}
