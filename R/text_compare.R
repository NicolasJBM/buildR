#' Transform the documents stored in a dataframe in a bag of words.
#' @param x     character string.
#' @param y     character string.
#' @return A tibble indicating elements of comparison between x and y contents.
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#' @export

text_compare <- function(x,y){
  
  inSource <- NULL
  inTarget <- NULL
  inBoth <- NULL
  
  x <- unlist(strsplit(x, split = " "))
  y <- unlist(strsplit(x, split = " "))
  
  overlap <- tibble::tibble(
    in_source = length(x),
    in_target = length(y),
    in_both = length(intersect(x,y))
  )
  
  return(overlap)
}
