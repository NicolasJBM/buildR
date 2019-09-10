#' Compute basic metrics for a bag of words.
#' @param x        tibble. Bag of words produced with text_anno2bow
#' @param source   character string. Name of the variable indicating the document id.
#' @param term     character string. Name of the variable indicating the term.
#' @param count    character string. Name of the variable indicating the frequence.
#' @return A tibble 9 variables: source, term, count, src_word_count (number of words in the source), term_count (frequency of the term), term_source_count (number of sources in which the term appears), tf, idf, tf_idf.
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr summarise_all
#' @importFrom dplyr %>%
#' @importFrom tidytext bind_tf_idf
#' @importFrom stats median
#' @export

text_word_metrics <- function(x,
                              source = "source",
                              term = "term",
                              count = "count") {
  bow <- x %>%
    select(source = source, term = term, count = count) %>%
    group_by(source, term) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()

  sourcecount <- bow %>%
    select(source, src_word_count = count) %>%
    group_by(source) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()

  termcount <- bow %>%
    select(term, term_count = count) %>%
    mutate(term_src_count = 1) %>%
    group_by(term) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()

  bow <- bow %>%
    left_join(sourcecount, by = "source") %>%
    left_join(termcount, by = "term") %>%
    bind_tf_idf(term, source, count)

  return(bow)
}
