#' Cluster observations using kmeans for either the specified number of subgroups or the number of unique observations.
#' @param x dataframe. characteristics of the observation to group.
#' @param gpnbr integer. Number of subgroups.
#' @return dataframe specifying group membership.
#' @importFrom stats kmeans
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @export


tble_make_subgroups <- function(x, gpnbr) {
  
  subgroup <- NULL
  
  gpnbr <- min(gpnbr, (nrow(unique(x))-1))
  if(gpnbr > 0){
    y <- data.frame(subgroup = kmeans(x, centers = gpnbr)$clust - 1)
  } else {
    y <- data.frame(subgroup = rep(0, nrow(x)))
  }
  
  y <- mutate(y, subgroup = as.character(subgroup))
  return(y)
}
