#' Count the occurrences of words associated with a dictionary.
#' @param x          String or character vector. Content to be scored.
#' @param type       Character. Whether x is a "string" (in which case it will be split) or a "vector"
#' @param dictionary Tibble. Categorizations of words.
#' @param category   Character. Name of the grouping variable in the dictionary.
#' @param pattern    Character. Name of the pattern variable in the dictionary.
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


text_score_dictionaries <- function(x,
                                    type = "vector",
                                    dictionary,
                                    category = NULL,
                                    pattern = NULL){
  
  group <- NULL
  word <- NULL
  score <- NULL
  
  if (type == "string") x <- unlist(strsplit(tolower(x), split = " ")) else x <- tolower(x)
  
  y <- dictionary %>%
    dplyr::rename(group = category, word = pattern) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(word = paste(word, collapse = "|")) %>%
    dplyr::mutate(
      score = purrr::map_dbl(
        word,
        function(word, text) sum(stringr::str_detect(text, word)),
        text = x
      )
    ) %>%
    dplyr::select(group, score) %>%
    tidyr::spread(group, score) %>%
    dplyr::mutate(total = length(x))
  
  return(y)
}
