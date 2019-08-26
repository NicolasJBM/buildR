#' Replace words from a list by another word
#' @param x            character vector. Texts to clean.
#' @param replacements dataframe. Table with two variables: "pattern" for the string to replace and "replacement" for the string to put instead.
#' @return A clean character vector.
#' @importFrom stringr str_replace_all
#' @export

replace_words <- function(x, replacements){
  if (!is.null(replacements)) {
    for (i in 1:nrow(replacements)) {
      x <- str_replace_all(x, pattern = replacements[i,1], replacement = replacements[i,2])
    }
  }
  return(x)
}