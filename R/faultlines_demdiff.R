#' Compute a score of demographic difference using the procedure suggested by Li and Hambrick (2005) for the study of faultlines.
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
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @export

faultlines_demdiff <- function(x, subgroup){
  
  # bind variables
  attribute <- NULL
  meanGp <- NULL
  meanSubGp <- NULL
  gp0 <- NULL
  gp1 <- NULL
  sd <- NULL
  sdSubGp <- NULL
  diffMean <- NULL
  prodSD <- NULL
  
  x <- x %>%
    dplyr::select(subgroup = subgroup, everything()) %>%
    mutate_if(is.factor, function(x) as.integer(as.character(x)))
  
  # Find the average values for each variable
  center <- x %>%
    summarise_all(mean) %>%
    gather(attribute, meanGp)
  
  # Find for each variable the centroid of each group
  centroids <- x %>%
    group_by(subgroup) %>%
    summarise_all(mean) %>%
    ungroup() %>%
    gather(attribute, meanSubGp, -subgroup)
  
  # Compute de Demographic Difference index of Li and Hambrick
  if (length(unique(centroids$subgroup)) > 1){
    
    #Compute the distances between subgroup centroids for each variable
    centroidsMeanDiff <- centroids %>%
      mutate(subgroup = paste0("gp", subgroup)) %>%
      spread(subgroup, meanSubGp, fill = NA) %>%
      mutate(diff = (gp0-gp1)^2) %>%
      dplyr::select(attribute, diffMean = diff)
    
    # Compute within subgroup standard deviations on each attribute and compute the demographic distance
    dem_diff <- x %>%
      group_by(subgroup) %>%
      summarise_all(stats::sd) %>%
      ungroup() %>%
      gather(attribute, sdSubGp, -subgroup) %>%
      mutate(subgroup = paste0("gp", subgroup)) %>%
      spread(subgroup, sdSubGp, fill = 0) %>%
      mutate(prod = ((gp0*gp1)/2)+1) %>%
      dplyr::select(attribute, prodSD = prod) %>%
      left_join(centroidsMeanDiff, by = "attribute") %>%
      mutate(dem_diff = diffMean / prodSD) %>%
      dplyr::select(dem_diff) %>%
      colMeans() %>%
      as.vector()
    
  } else dem_diff <- as.vector(NA)
  names(dem_diff) <- c("DemDiff")
  
  results <- data.frame(as.list(c(dem_diff)))
  
  return(results)
  
}