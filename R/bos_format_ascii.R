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

bos_format_ascii <- function(text){
  
  text <- text %>%
    stringr::str_replace_all("<.*?>", " ") %>% # remove html tags
    stringr::str_replace_all("  ", " ") %>% # replace double spaces by single spaces
    stringr::str_split(" ") %>%
    unlist()
  
  split = strsplit(text, split='')
  m = lapply(split,match, buildR::bos_toascii$mapL)
  mapply(function(split,m) paste(ifelse(is.na(m), split, buildR::bos_toascii$mapA[m]), collapse='') , split, m)
  
  text <- text %>%
    stringi::stri_trans_general(id = "ascii") %>%
    textclean::replace_non_ascii(replacement = "", remove.nonconverted = TRUE)
  
  text <- paste(text, collapse = " ")
  
  trimws(text)
}
