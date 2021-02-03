#' @name faultlines_fau
#' @title Compute FAU distances between subgroups
#' @author Nicolas Mangin
#' @description Compute distances within and between sub-groups according to the procedure designed by Thatcher, Jehn, and Zanutto (2003) for faultlines analyses.
#' @param x dataframe. Table informing about team members characteristics, numeric or binary. Ideally, all values should be scaled between 0 and 1 before running the analysis.
#' @param subgroup character. Name of the variable indicating subgroups memberships.
#' @return A dataframe specifying the distances between subgroups centroids (FAU_CGD), the distances within sub-group (FAU_SGH), the distances within the whole group (FAU_GH) the proximity between sub-groups (DSG_CGA_'subgp_base'), the faultline score (FAU_strength) and the faultline distance (FAU_dist)
#' @references Thatcher, S. M. B., K. A. Jehn, and E. Zanutto. 2003. Cracks in Diversity Research: The Effects of Diversity Faultlines on Conflict and Performance. Group Decision and Negotiation 12 (3): 217–241.
#' @seealso make_0_to_1
#' @seealso find_subgroups
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @importFrom dplyr everything
#' @importFrom dplyr summarise_all
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_if
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @export

faultlines_fau <- function(x, subgroup) {

  # Bind variables
  meanGp <- NULL
  meanSubGp <- NULL
  position <- NULL
  FAU_CGD <- NULL
  FAU_SGH <- NULL
  FAU_GH <- NULL
  gp0 <- NULL
  gp1 <- NULL
  data <- NULL

  x <- x %>%
    dplyr::select(subgroup = subgroup, dplyr::everything()) %>%
    dplyr::mutate_if(is.factor, function(x) as.integer(as.character(x)))

  # Find the average values for each variable
  center <- x %>%
    dplyr::summarise_all(mean) %>%
    tidyr::pivot_longer(
      names_to = "attribute",
      values_to = "meanGp"
    )

  # Find for each variable the centroid of each group
  centroids <- x %>%
    dplyr::group_by(subgroup) %>%
    dplyr::summarise_all(mean) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = setdiff(names(x), "subgroup"),
      names_to = "attribute",
      values_to = "meanSubGp"
    )

  # Compute for each team member its distances to the group and
  # subgroup centroids for each variable
  # CGD is the distance between subgroup centroids
  # SGH is the distance between individuals and
  # their respective subgroup centroid
  # GH is the distance between individuals and the group centroid
  inddist <- x %>%
    tidyr::pivot_longer(
      cols = setdiff(names(x), "subgroup"),
      names_to = "attribute",
      values_to = "position"
    ) %>%
    dplyr::left_join(center, by = "attribute") %>%
    dplyr::left_join(centroids, by = c("attribute", "subgroup")) %>%
    dplyr::mutate(
      FAU_CGD = (meanSubGp - meanGp)^2,
      FAU_SGH = (position - meanSubGp)^2,
      FAU_GH = (position - meanGp)^2
    ) %>%
    dplyr::select(FAU_CGD, FAU_SGH, FAU_GH) %>%
    colSums()

  # Compute the FAU score (proportion of all differences explained by
  # the difference between subgroup centroids)
  fau_score <- as.vector(inddist["FAU_CGD"] / inddist["FAU_GH"])
  names(fau_score) <- c("FAU_strength")

  # Compute the faultline distance
  if (length(unique(centroids$subgroup)) > 1) {
    fau_dist <- centroids %>%
      dplyr::mutate(subgroup = paste0("gp", subgroup)) %>%
      tidyr::pivot_wider(
        names_from = "subgroup",
        values_from = "meanSubGp",
        values_fill = NA
      ) %>%
      dplyr::mutate(diff = (gp0 - gp1)^2) %>%
      dplyr::select(fau_dist = diff)
    fau_dist <- sqrt(colSums(fau_dist))
  } else {
    fau_dist <- as.vector(NA)
  }
  names(fau_dist) <- c("FAU_dist")

  results <- data.frame(as.list(c(inddist, fau_score, fau_dist)))

  return(results)
}
