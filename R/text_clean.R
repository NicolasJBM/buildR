#' Remove html tags, remove double spaces, convert to ASCII.
#' @param text  character. String to be cleaned.
#' @return A clean string.
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#' @importFrom tm stripWhitespace
#' @importFrom stringr str_split
#' @importFrom stringi stri_trans_general
#' @importFrom textclean replace_non_ascii
#' @export

text_clean <- function(text){
  
  stopifnot(
    is.character(text),
    length(text) == 1
  )
  
  text %>%
    as.character() %>%
    buildR::bos_format_ascii() %>%
    tm::stripWhitespace() %>%
    trimws()
}
