#' @name faultlines_fls
#' @title Compute faultline scores between subgroups
#' @author Nicolas Mangin
#' @description Computes the distances within and between sub-groups according to the procedure designed by Shaw (2004) for faultline analyses.
#' @param x dataframe. Binary variables indicating the categorical memberships of group members.
#' @param subgroup character. Name of the variable informing about subgroup membership.
#' @return A dataframe specifying the subgroups internal alignement (FLS_IA_0, FLS_IA_1, FLS_IA), the cross-subgroups alignment (FLS_CGA), and the faultline score (FLS).
#' @seealso make_discrete
#' @seealso find_subgroups
#' @references Shaw, J. B. 2004. The Development and Analysis of a Measure of Group Faultlines. Organizational Research Methods 7 (1): 66–100.
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom dplyr summarise_all
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr left_join
#' @importFrom dplyr summarise_all
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom forcats fct_recode
#' @importFrom stats ftable
#' @export


faultlines_fls <- function(x, subgroup) {

  # Bind variables
  data <- NULL

  x <- x %>%
    dplyr::select(subgroup = subgroup, dplyr::everything()) %>%
    dplyr::mutate_if(is.factor, function(x) as.integer(as.character(x)))

  alignments <- x %>%
    dplyr::group_by(subgroup) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(data, fls_ia)) %>%
    tidyr::unnest(data) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(subgroup = paste("FLS_IA", subgroup, sep = "_")) %>%
    tidyr::pivot_wider(
      names_from = "subgroup",
      values_from = "data",
      values_fill = NA
    )

  # Compute faultlines
  if (length(unique(x$subgroup)) > 1) {
    alignments <- alignments %>%
      dplyr::mutate(FLS_CGA = grp_fls_cga(x))
    alignments[, "FLS_IA"] <- (alignments[[1, 1]] + alignments[[1, 2]]) / 2
    alignments[, "FLS"] <- (alignments[[1, 4]] * (1 - alignments[[1, 3]]))
  } else {
    alignments <- data.frame(
      FLS_IA_0 = alignments[[1]],
      FLS_IA_1 = NA,
      FLS_CGA = NA,
      FLS_IA = alignments[[1]],
      FLS = 0
    )
  }

  # Return results
  return(as.data.frame(alignments))
}



fls_ia <- function(subgp) {
  subgp <- as.data.frame(subgp)

  int_align <- function(var) {

    # Compute basic metrics used in computation of the IA score
    nbrObs <- length(var)
    expect <- nbrObs / 2
    maxVal <- ((nbrObs - expect)^2 / expect + (0 - expect)^2 / expect)
    minVal <- (
      (round(expect) - expect)^2 / expect +
        (nbrObs - round(expect) -
          expect)^2 / expect
    )
    maxDiff <- (maxVal - minVal)

    # Internal alignement of a subgroup
    var <- ((
      (sum(var) - expect)^2 / expect +
        (nbrObs - sum(var) - expect)^2 / expect
    ) - minVal) / maxDiff
  }

  if (nrow(subgp) > 1) {
    subgp_IA <- subgp %>%
      dplyr::summarise_all(int_align) %>%
      rowMeans()
  } else {
    subgp_IA <- 1
  }

  return(subgp_IA)
}


grp_fls_cga <- function(group) {

  # Bind variables
  cat0 <- NULL
  cat1 <- NULL
  subgroup <- NULL

  nbrCat1 <- sum(group$subgroup)
  nbrCat0 <- nrow(group) - nbrCat1
  possibleCross <- nbrCat0 * nbrCat1
  crosscat <- c()

  if (nbrCat1 > 0 & nbrCat0 > 0) {
    for (i in 2:length(group)) {
      tmp <- as.data.frame(
        stats::ftable(
          dplyr::select(group, subgroup, names(group)[i])
        )
      ) %>%
        dplyr::mutate(subgroup = as.factor(subgroup)) %>%
        dplyr::mutate(
          subgroup = forcats::fct_recode(
            subgroup,
            cat0 = "0", cat1 = "1"
          )
        ) %>%
        tidyr::pivot_wider(
          names_from = "subgroup",
          values_from = "Freq",
          fill = 0
        ) %>%
        dplyr::mutate(crossProduct = cat0 * cat1)
      crosscat[i - 1] <- sum(tmp$crossProduct) / possibleCross
    }
    CGA <- mean(crosscat)
  } else {
    CGA <- NA
  }

  return(CGA)
}
