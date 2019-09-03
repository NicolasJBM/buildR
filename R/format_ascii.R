#' Make sure all characters are ASCII
#' @param x  character vector. Texts to clean.
#' @return A clean character vector.
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringi stri_trans_general
#' @importFrom textclean replace_non_ascii
#' @export

format_ascii <- function(x){
  
  x <- x %>%
    str_replace_all("<.*?>", " ") %>% # remove html tags
    str_replace_all("  ", " ") %>% # replace double spaces by single spaces
    str_split(" ") %>%
    unlist()
  
  split=strsplit(x,split='')
  m=lapply(split,match, buildR::toascii$mapL)
  mapply(function(split,m) paste(ifelse(is.na(m),split,buildR::toascii$mapA[m]),collapse='') , split, m)
  
  x <- x %>%
    stringi::stri_trans_general(id = "ascii") %>%
    replace_non_ascii(replacement = "", remove.nonconverted = TRUE)
  
  x <- paste(x, collapse = " ")
  
  trimws(x)
}