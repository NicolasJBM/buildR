#' Compute distances within and between sub-groups according to the procedure designed by Thatcher, Jehn, and Zanutto (2003) for faultlines analyses.
#' @param x dataframe. Table informing about team members characteristics, numeric or binary. Ideally, all values should be scaled between 0 and 1 before running the analysis.
#' @param subgroup character. Name of the variable indicating subgroups memberships.
#' @return A dataframe specifying the distances between subgroups centroids (FAU_CGD), the distances within sub-group (FAU_SGH), the distances within the whole group (FAU_GH) the proximity between sub-groups (DSG_CGA_'subgp_base'), the faultline score (FAU_strength) and the faultline distance (FAU_dist)
#' @references Thatcher, S. M. B., K. A. Jehn, and E. Zanutto. 2003. Cracks in Diversity Research: The Effects of Diversity Faultlines on Conflict and Performance. Group Decision and Negotiation 12 (3): 217–241.
#' @examples
#' library(construct)
#' data("fictiveteams")
#' # prepare the data: group the observations per team, scale between 0 and 1, identify subgroups
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#' fl <- fictiveteams %>%
#'    group_by(team) %>%
#'    nest() %>%
#'    mutate(data = map(data, make_0_to_1)) %>%
#'    mutate(subgp = map(data, find_subgroups, gpnbr = 2)) %>%
#'    unnest()
#' # Compute demographic differences
#' fl %>%
#'    group_by(team) %>%
#'    nest() %>%
#'    mutate(data = map(data, faultlines_fau, subgroup = "subgroup")) %>%
#'    unnest()
#' @seealso make_0_to_1
#' @seealso find_subgroups
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @importFrom dplyr everything
#' @importFrom dplyr summarise_all
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_if
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @export

faultlines_fau <- function(x, subgroup){
  
  # Bind variables
  attribute <- NULL
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
    gather(attribute, meanSubGp, -subgroup)
  
  # Compute for each team member its distances to the group and subgroup centroids for each variable
  # CGD is the distance between subgroup centroids
  # SGH is the distance between individuals and their respective subgroup centroid
  # GH is the distance between individuals and the group centroid
  inddist <- x %>%
    gather(attribute, position, -subgroup) %>%
    left_join(center, by = "attribute") %>%
    left_join(centroids, by = c("attribute", "subgroup")) %>%
    mutate(
      FAU_CGD = (meanSubGp - meanGp)^2,
      FAU_SGH = (position - meanSubGp)^2,
      FAU_GH = (position - meanGp)^2
    ) %>%
    dplyr::select(FAU_CGD, FAU_SGH, FAU_GH) %>%
    colSums()
  
  # Compute the FAU score (proportion of all differences explained by the difference between subgroup centroids)
  fau_score <- as.vector(inddist["FAU_CGD"] / inddist["FAU_GH"])
  names(fau_score) <- c("FAU_strength")
  
  # Compute the faultline distance
  if (length(unique(centroids$subgroup)) > 1){
    fau_dist <- centroids %>%
      mutate(subgroup = paste0("gp", subgroup)) %>%
      spread(subgroup, meanSubGp, fill = NA) %>%
      mutate(diff = (gp0-gp1)^2) %>%
      dplyr::select(fau_dist = diff)
    fau_dist <- sqrt(colSums(fau_dist))
  } else fau_dist <- as.vector(NA)
  names(fau_dist) <- c("FAU_dist")
  
  results <- data.frame(as.list(c(inddist, fau_score, fau_dist)))
  
  return(results)
  
}
