#' Count the number of words in a text.
#' @param text Character. The sentence(s) in which the words have to be counted.
#' @return Numeric. Number of words in the text.
#' @importFrom purrr map
#' @importFrom buildR bos_format_ascii
#' @importFrom stringr str_split
#' @importFrom utils read.table
#' @importFrom clipr read_clip
#' @export


count_words <- function(text = "") {
  if (text == "") {
    text <- clipr::read_clip()
  } else {
    text <- text
  }
  text <- unlist(purrr::map(text, stringr::str_split, pattern = " "))
  text <- purrr::map(text, buildR::bos_format_ascii)
  text <- unlist(text[text != ""])
  length(text)
}
