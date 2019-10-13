#' Remove html tags, remove double spaces, convert to ASCII.
#' @param text  character. String to be cleaned.
#' @param clndash logical. Whether spaces should be removed around dashes.
#' @param clnpunct logical. Whether punctuations should be properly formated (no space before, space after).
#' @param clnascii logical. Whether non-ascii characters should be replaced by their closest ascii match.
#' @param clnspc logical. Trim white spaces and prevent double-spaces.
#' @return A clean string.
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#' @importFrom tm stripWhitespace
#' @importFrom stringr str_split
#' @importFrom stringr str_replace_all
#' @importFrom stringr fixed
#' @importFrom stringi stri_trans_general
#' @importFrom textclean replace_non_ascii
#' @export

text_clean <- function(text,
                       clndash = TRUE,
                       clnpunct = TRUE,
                       clnascii = TRUE,
                       clnspc = TRUE){
  
  stopifnot(
    is.character(text),
    length(text) == 1
  )
  
  clean <- text
  
  if (clndash){
    clean <- clean %>%
      stringr::str_replace_all("-\\s([A-Za-z])", "-\\1") %>%
      stringr::str_replace_all("\\s-([A-Za-z])", "-\\1") %>%
      as.character()
  }
  
  if (clnpunct){
    clean <- clean %>%
      stringr::str_replace_all("\\.([A-Za-z])", ". \\1") %>%
      stringr::str_replace_all("\\;([A-Za-z])", "; \\1") %>%
      stringr::str_replace_all("\\,([A-Za-z])", ", \\1") %>%
      stringr::str_replace_all("\\:([A-Za-z])", ": \\1") %>%
      stringr::str_replace_all("\\!([A-Za-z])", "! \\1") %>%
      stringr::str_replace_all("\\?([A-Za-z])", "? \\1") %>%
      stringr::str_replace_all(stringr::fixed(" ."), stringr::fixed(".")) %>%
      stringr::str_replace_all(stringr::fixed(" ;"), stringr::fixed(";")) %>%
      stringr::str_replace_all(stringr::fixed(" ,"), stringr::fixed(",")) %>%
      stringr::str_replace_all(stringr::fixed(" :"), stringr::fixed(":")) %>%
      stringr::str_replace_all(stringr::fixed(" !"), stringr::fixed("!")) %>%
      stringr::str_replace_all(stringr::fixed(" ?"), stringr::fixed("?")) %>%
      as.character()
  }
  
  if (clnascii){
    clean <- clean %>%
      buildR::bos_format_ascii() %>%
      as.character()
  }
  
  if (clnspc){
    clean <- clean %>%
      tm::stripWhitespace() %>%
      trimws() %>%
      as.character()
  }
  
  return(clean)  
}
