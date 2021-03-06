#' @name net_aggredge
#' @title Aggregate edges
#' @author Nicolas Mangin
#' @description Aggregate edges according to specifications.
#' @param edgelist Tibble. Edgelist with four variables: year, from, to, weight
#' @param mode     Character. Indicate whether edges should be aggregrated as "full", "cumulative", or "rolling".
#' @param step     Numeric. Size of the increments in years.
#' @param range    Numeric. In the "rolling" mode, this refers to the size of the window.
#' @return An aggregated edgelist.
#' @importFrom tibble is_tibble
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr n
#' @importFrom tidyr unnest
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @export


net_aggredge <- function(edgelist, mode = "rolling", step = 1, range = 1) {
  stopifnot(
    tibble::is_tibble(edgelist),
    length(intersect(c("year", "from", "to", "weight"), names(edgelist))) == 4,
    mode %in% c("full", "cumulative", "rolling")
  )

  year <- NULL
  from <- NULL
  to <- NULL
  weight <- NULL
  start <- NULL
  end <- NULL
  edges <- NULL

  if (mode == "full") {
    aggregation <- tibble::tibble(
      start = min(edgelist$year),
      end = max(edgelist$year)
    )
  } else if (mode == "cumulative") {
    aggregation <- tibble::tibble(
      start = min(edgelist$year),
      end = seq(from = min(edgelist$year), to = max(edgelist$year), by = step)
    )
  } else {
    aggregation <- tibble::tibble(
      start = seq(
        from = min(edgelist$year),
        to = max(edgelist$year) - range + 1,
        by = step
      ),
      end = seq(
        from = min(edgelist$year) + range - 1,
        to = max(edgelist$year),
        by = step
      )
    )
  }

  aggregate_edges <- function(x, y, z) {
    z %>%
      dplyr::filter(year >= x, year <= y) %>%
      dplyr::select(from, to, weight) %>%
      dplyr::group_by(from, to) %>%
      dplyr::summarise(weight = sum(weight), count = dplyr::n()) %>%
      dplyr::ungroup()
  }

  fulledgelist <- edgelist %>%
    tidyr::pivot_wider(
      names_from = "to",
      values_from = "weight",
      values_fill = 0
    )
  fulledgelist <- fulledgelist %>%
    tidyr::pivot_longer(
      cols = setdiff(names(fulledgelist), c("year", "from")),
      names_to = "to",
      values_to = "weight"
    ) %>%
    tidyr::pivot_wider(
      names_from = "from",
      values_from = "weight",
      values_fill = 0
    )
  fulledgelist <- fulledgelist %>%
    tidyr::pivot_longer(
      cols = setdiff(names(fulledgelist), c("year", "to")),
      names_to = "from",
      values_to = "weight"
    )

  aggregation <- aggregation %>%
    dplyr::mutate(edges = purrr::map2(
      start,
      end,
      aggregate_edges,
      z = fulledgelist
    )) %>%
    tidyr::unnest(edges) %>%
    dplyr::ungroup()

  return(aggregation)
}
