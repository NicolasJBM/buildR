#' Count the occurrences of words associated with a dictionary.
#' @param x          String or character vector. Conte to be scored.
#' @param dictionary Tibble. Categorizations of words.
#' @param type       Character. Whether x is a "string" (in which case it will be split) or a "vector"
#' @param level      Character. Name of the grouping variable in the dictionary.
#' @return A tibble with the count of words, for each category and in total, forht the document.
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom purrr map_dbl
#' @importFrom stringr str_detect
#' @importFrom tidyr spread
#' @importFrom tibble tibble
#' @export


text_score_sentiment <- function(x, dictionary, type = "vector", level = "cluster"){
  
  group <- NULL
  words <- NULL
  score <- NULL
  
  if (type == "string") x <- unlist(strsplit(tolower(x), split = " ")) else x <- tolower(x)
  
  y <- dictionary %>%
    dplyr::rename(group = level) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(words = paste(words, collapse = "|")) %>%
    dplyr::mutate(
      score = purrr::map_dbl(
        words,
        function(words, text) sum(stringr::str_detect(text, words)),
        text = x
      )
    ) %>%
    dplyr::select(group, score) %>%
    tidyr::spread(group, score) %>%
    dplyr::mutate(total = length(x))
  
  return(y)
}
