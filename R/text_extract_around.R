#' Extract windows of words around a focal worj
#' @param text        character. Text.
#' @param term        character. regex pattern around which the adjacent pattern has to be detected within the window.
#' @param window      numeric vector. Two values: one indicating the beginning of the window from the focal pattern, and one indicating the end.
#' @return A list of windows around each occurrences of the focal term.
#' @importFrom stringr str_extract_all
#' @export


text_extract_around <- function(text, term, window = c(3,3)){
  expression <- paste0("([^\\s]+\\s){0,", window[1],"}",term,".\\w+(\\s[^\\s]+){0,", (window[2]-1),"}")
  extraction <- unlist(stringr::str_extract_all(text, expression))
  if (length(extraction) == 0) extraction <- ""
  return(extraction)
}