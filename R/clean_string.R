#' Simplify and clean a text.
#' @param x            character. Text to be cleaned.
#' @param language     character string. Language for the stopwords to remove.
#' @return A clean text.
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#' @importFrom tm removeNumbers
#' @importFrom tm removePunctuation
#' @importFrom tm stripWhitespace
#' @export

clean_string <- function(x){
  x %>%
    as.character() %>%
    tolower() %>%
    stringr::str_replace_all(pattern = "-", replacement = " ") %>%
    tm::removeNumbers() %>%
    tm::removePunctuation() %>%
    tm::stripWhitespace() %>%
    trimws()
}
