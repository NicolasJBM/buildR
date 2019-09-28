#' Count the occurrences of words associated with a dictionary.
#' @param bow        Tibble. Bag of words for one document with at least two variables: "term" and "count".
#' @param dictionary Tibble. Categorizations of words with at least three variables: "term", "category", and "value".
#' @param score      Character. Whether the function should return a "total" or a "percentage".
#' @return A tibble with documents as rows, categories as variables, ad the required summary in cells
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom fuzzyjoin regex_inner_join
#' @importFrom dplyr left_join
#' @importFrom tidyr spread
#' @importFrom tibble is_tibble
#' @export


text_apply_dictionaries <- function(bow,
                                    dictionary,
                                    score = "total"
                                    ){
  
  stopifnot(
    length(intersect(c("term","count"), names(bow))) == 2,
    length(intersect(c("term","category","value"), names(dictionary))) == 3,
    score %in% c("total","percentage")
  )
  
  term <- NULL
  category <- NULL
  value <- NULL
  total <- NULL
  
  
  bow <- dplyr::select(bow, term, count) %>%
    dplyr::mutate(term = as.character(term))
  dictionary <- dplyr::select(dictionary, term, category, value) %>%
    dplyr::mutate(term = as.character(term))
  
  nbr_words <- sum(bow$count)
  
  y <- bow %>%
    fuzzyjoin::regex_inner_join(dictionary, by = "term", ignore_case = TRUE) %>%
    dplyr::mutate(total = count * value) %>%
    dplyr::select(category, total) %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(total = sum(total, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percentage = total / nbr_words) %>%
    dplyr::select(category, score = score) %>%
    tidyr::spread(category, score)
  
  return(y)
}
