#' Remove duplicated entries. If there are several discrepant pieces of information about the observation, take the most frequent unique value, or the mean, or a random value of the duplicates to keep only one observation.
#' @param x dataframe. Variables indicating the attributes of what should be a single observation (mapped after nesting by id).
#' @return Dataframe with only one observation.
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate_all
#' @importFrom dplyr summarise_all
#' @export

tble_make_unique <- function(x){
  if (nrow(x) > 1){
    x <- x %>%
      dplyr::mutate_all(function(x){
        keep_class <- class(x)
        x <- table(x)
        if (length(x) == 0) x <- NA else x <- names(sort(x, decreasing=TRUE))[1]
        class(x) <- keep_class
        x
      }) %>%
      unique()
  } else if (nrow(x) == 1) x <- x else x <- dplyr::summarise_all(x, function(x) NA)
  return(x)
}