#' Remove attributes of all the variables.
#' @param x dataframe.
#' @return Dataframe without attributes.
#' @importFrom tibble as_tibble
#' @export

tble_remove_attributes <- function(x){
  x[] <- lapply(x, function(x) { attributes(x) <- NULL; x })
  x <- tibble::as_tibble(x)
  return(x)
}