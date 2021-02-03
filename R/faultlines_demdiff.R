#' @name faultlines_demdiff
#' @title Compute demographic difference between subgroups
#' @author Nicolas Mangin
#' @description Compute a score of demographic difference using the procedure suggested by Li and Hambrick (2005) for the study of faultlines.
#' @param x dataframe. Table informing about team members' characteristics (all team members should belong to the same team), numeric or binary. Ideally, all values should be scaled between 0 and 1 before running the analysis.
#' @param subgroup character. Name of the variable indicating subgroups memberships.
#' @return A dataframe with one row (for the whole group) containing the index of demographic difference (DemDiff).
#' @references Li, J., and D. C. Hambrick. 2005. Factional groups: A new vantage on demographic faultlines, conflict, and disintegration in work teams. Academy of Management Journal 48 (5): 794–813.
#' @seealso make_0_to_1
#' @seealso find_subgroups
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom dplyr summarise_all
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @export

faultlines_demdiff <- function(x, subgroup) {

  # bind variables
  attribute <- NULL
  gp0 <- NULL
  gp1 <- NULL
  diffMean <- NULL
  prodSD <- NULL

  x <- x %>%
    dplyr::select(subgroup = subgroup, dplyr::everything()) %>%
    dplyr::mutate_if(is.factor, function(x) as.integer(as.character(x)))

  # Find the average values for each variable
  center <- x %>%
    dplyr::summarise_all(mean) %>%
    tidyr::pivot_longer(names_to = "attribute", values_to = "meanGp")

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

  # Compute de Demographic Difference index of Li and Hambrick
  if (length(unique(centroids$subgroup)) > 1) {

    # Compute the distances between subgroup centroids for each variable
    centroidsMeanDiff <- centroids %>%
      dplyr::mutate(subgroup = paste0("gp", subgroup)) %>%
      tidyr::pivot_wider(
        names_from = "subgroup",
        values_from = "meanSubGp",
        values_fill = NA
      ) %>%
      dplyr::mutate(diff = (gp0 - gp1)^2) %>%
      dplyr::select(attribute, diffMean = diff)

    # Compute within subgroup standard deviations on each attribute and
    # compute the demographic distance
    dem_diff <- x %>%
      dplyr::group_by(subgroup) %>%
      dplyr::summarise_all(stats::sd) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        cols = setdiff(names(x), "subgroup"),
        names_to = "attribute",
        values_to = "sdSubGp"
      ) %>%
      dplyr::mutate(subgroup = paste0("gp", subgroup)) %>%
      tidyr::pivot_wider(
        names_from = "subgroup",
        values_from = "sdSubGp",
        values_fill = 0
      ) %>%
      dplyr::mutate(prod = ((gp0 * gp1) / 2) + 1) %>%
      dplyr::select(attribute, prodSD = prod) %>%
      dplyr::left_join(centroidsMeanDiff, by = "attribute") %>%
      dplyr::mutate(dem_diff = diffMean / prodSD) %>%
      dplyr::select(dem_diff) %>%
      colMeans() %>%
      as.vector()
  } else {
    dem_diff <- as.vector(NA)
  }
  names(dem_diff) <- c("DemDiff")

  results <- data.frame(as.list(c(dem_diff)))

  return(results)
}
