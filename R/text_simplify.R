#' Simplify text to facilitate comparisons of strings
#' @param text  character. String to be simplified
#' @return A siplified string.
#' @importFrom dplyr %>%
#' @importFrom tm removePunctuation
#' @importFrom tm removeNumbers
#' @export

text_simplify <- function(text){
  text %>%
    tm::removePunctuation() %>%
    tm::removeNumbers() %>%
    buildR::text_clean() %>%
    tolower()
}
