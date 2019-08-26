#' Cluster observations using kmeans for either the specified number of subgroups or the number of unique observation.
#' @param x dataframe. characteristics of the observation to group.
#' @param gpnbr integer. Number of subgroups.
#' @return dataframe specifying group membership.
#' @importFrom stats kmeans
#' @importFrom dplyr case_when
#' @export


find_subgroups <- function(x, gpnbr) {
  gpnbr <- min(gpnbr, (nrow(unique(x))-1))
  if(gpnbr > 0){
    data.frame(subgroup = kmeans(x, centers = gpnbr)$clust - 1)
  } else {
    data.frame(subgroup = rep(0, nrow(x)))
  }
}
