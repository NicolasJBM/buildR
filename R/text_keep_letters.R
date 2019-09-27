#' Remove all characthers but letters.
#' @param text  character. String to be cleaned.
#' @return A clean string.
#' @importFrom dplyr %>%
#' @importFrom stringr str_remove_all
#' @export

text_keep_letters <- function(text){
  text %>%
    stringr::str_remove_all("[0-9]") %>%
    stringr::str_remove_all("[,:!;?-]") %>%
    stringr::str_remove_all("\\.") %>%
    stringr::str_remove_all(buildR::bos_symbols) %>%
    stringr::str_remove_all("'") %>%
    stringr::str_remove_all('"')
}

