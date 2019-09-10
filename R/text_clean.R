#' Simplify and clean a text.
#' @param x  character. String to be cleaned.
#' @return A clean string.
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#' @importFrom tm stripWhitespace
#' @importFrom stringr str_split
#' @importFrom stringi stri_trans_general
#' @importFrom textclean replace_non_ascii
#' @export

text_clean <- function(x){
  
  format_ascii <- function(x){
    
    x <- x %>%
      stringr::str_replace_all("<.*?>", " ") %>% # remove html tags
      stringr::str_replace_all("  ", " ") %>% # replace double spaces by single spaces
      stringr::str_split(" ") %>%
      unlist()
    
    split = strsplit(x,split='')
    m = lapply(split,match, buildR::bos_toascii$mapL)
    mapply(function(split,m) paste(ifelse(is.na(m),split,buildR::bos_toascii$mapA[m]),collapse='') , split, m)
    
    x <- x %>%
      stringi::stri_trans_general(id = "ascii") %>%
      textclean::replace_non_ascii(replacement = "", remove.nonconverted = TRUE)
    
    x <- paste(x, collapse = " ")
    
    trimws(x)
  }
  
  x %>%
    as.character() %>%
    format_ascii() %>%
    stringr::str_replace_all(pattern = "-", replacement = " ") %>%
    tm::stripWhitespace() %>%
    trimws()
}
