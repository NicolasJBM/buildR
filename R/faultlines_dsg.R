#' Computes the distances within and between sub-groups based on the number of common attributes (how many times members of a subgroup fall in the same category) for faultline analyses (personal procedure)
#' @param x dataframe. Binary variables indicating the categorical memberships of group members.
#' @param subgroup character. Name of the variable indicating subgroup membership.
#' @return A dataframe with one row per attribute specifying the contribution of each attribute to subgroups internal alignements, cross group alignement, and faultline.
#' @seealso make_discrete
#' @seealso find_subgroups
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr everything
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr desc
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom tidyr gather
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @export

faultlines_dsg <- function(x, subgroup){
  
  # bind variables
  attribute <- NULL
  value <- NULL
  data <- NULL
  DSG_IA_0 <- NULL
  DSG_IA_1 <- NULL
  DSG_CGA <- NULL
  DSG_IA <- NULL
  attribute <- NULL
  
  x <- x %>%
    dplyr::select(subgroup = subgroup, everything()) %>%
    mutate_if(is.factor, function(x) as.integer(as.character(x)))
  
  #Compute basic headcounts
  nbr <- nrow(x)
  nbrSGP1 <- sum(x$subgroup)
  nbrSGP0 <- nbr - nbrSGP1
  nbrAttr <- length(x)
  
  # Prepare computation of proximities
  comp_proxim <- function(x, type="within") {
    if (type == "within") maximum <- length(x)^2-length(x) else maximum <- length(x) * nrow(x)
    if (maximum == 0) maximum <- 1
    1-sum(x)/maximum
  }
  
  #Compute distances in terms of number of attributes
  distances <- x %>%
    arrange(desc(subgroup)) %>%
    dplyr::select(-subgroup) %>%
    gather(attribute, value) %>%
    group_by(attribute) %>%
    nest() %>%
    mutate(data = map(data, dist, method = "manhattan")) %>%
    mutate(data = map(data, as.matrix)) %>%
    mutate(data = map(data, as.data.frame))
  
  # Compute scores
  if (nbrSGP1 == 0){
    proximities <- distances %>%
      mutate(
        DSG_IA_1 = NA,
        DSG_IA_0 = map(data, comp_proxim, type = "within"),
        DSG_CGA = NA
      ) %>%
      dplyr::select(-data) %>%
      unnest() %>%
      mutate(DSG_IA = DSG_IA_0) %>%
      mutate(DSG = NA)
  } else if (nbrSGP0 == 0){
    proximities <- distances %>%
      mutate(
        DSG_IA_1 = map(data, comp_proxim, type = "within"),
        DSG_IA_0 = NA,
        DSG_CGA = NA
      ) %>%
      dplyr::select(-data) %>%
      unnest() %>%
      mutate(DSG_IA = DSG_IA_1) %>%
      mutate(DSG = NA)
  } else{
    proximities <- distances %>%
      mutate(
        DSG_IA_1 = map(data, dplyr::select, 1:nbrSGP1),
        DSG_IA_0 = map(data, dplyr::select, (nbrSGP1+1):nbr),
        DSG_CGA = map(data, dplyr::select, (nbrSGP1+1):nbr)
      ) %>%
      mutate(
        DSG_IA_1 = map(DSG_IA_1, slice, 1:nbrSGP1),
        DSG_IA_0 = map(DSG_IA_0, slice, (nbrSGP1+1):nbr),
        DSG_CGA = map(DSG_CGA, slice, 1:nbrSGP1)
      ) %>%
      mutate(
        DSG_IA_1 = map(DSG_IA_1, comp_proxim, type = "within"),
        DSG_IA_0 = map(DSG_IA_0, comp_proxim, type = "within"),
        DSG_CGA = map(DSG_CGA, comp_proxim, type = "CGA")
      ) %>%
      dplyr::select(-data) %>%
      unnest() %>%
      mutate(DSG_IA = DSG_IA_0/2 + DSG_IA_1/2) %>%
      mutate(DSG = DSG_IA * (1-DSG_CGA))
  }
  
  # Gather and return results
  results <- proximities
  return(results)
  
}

