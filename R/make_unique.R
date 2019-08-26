#' Remove duplicated entries. If there are still several instances of the same value in the id column, take the most frequent unique value, or the mean, or a random value of the duplicates to keep only one observation.
#' @param x dataframe. Variables indicating the attributes of observations.
#' @return Dataframe with only one instance of each id.
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate_all
#' @importFrom dplyr summarise_all
#' @export

make_unique <- function(x){
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