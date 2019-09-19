#' Identify linebreaks which do not seem to match an end of paragraph and recompose the paragraph.
#' @param text       Character. A string or vector of strings in which patterns have to be replaced.
#' @param dictionary Tibble. A tibble with two variables: "pattern" and "replacement".
#' @return The initial string or vector in which replacements have been made.
#' @importFrom tibble is_tibble
#' @importFrom stringr str_replace_all
#' @export


text_replace_words <- function(text, dictionary){
  
  stopifnot(
    is.character(text),
    tibble::is_tibble(dictionary)
  )
  
  y <- text
  
  for (i in 1:nrow(dictionary)){
    y <- stringr::str_replace_all(y, dictionary$pattern[[i]], dictionary$replacement[[i]])
  }
  
  return(y)
}