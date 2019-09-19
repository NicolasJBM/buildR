#' Compute basic metrics for a bag of words.
#' @param bow        tibble. Bag of words produced with text_semrel2bow
#' @param document   character string. Name of the variable indicating the document id.
#' @param term     character string. Name of the variable indicating the term.
#' @param count    character string. Name of the variable indicating the frequence.
#' @return A tibble 9 variables: document, term, count, doc_word_count (number of words in the document), term_count (frequency of the term), term_document_count (number of documents in which the term appears), tf, idf, tf_idf.
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

text_bow_metrics <- function(bow,
                             document = "document",
                             term = "term",
                             count = "count") {
  
  stopifnot(
    tibble::is_tibble(bow),
    is.character(document),
    is.character(term),
    is.character(count)
  )
  
  bow <- bow %>%
    select(document = document, term = term, count = count) %>%
    group_by(document, term) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()

  documentcount <- bow %>%
    select(document, doc_word_count = count) %>%
    group_by(document) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()

  termcount <- bow %>%
    select(term, term_count = count) %>%
    mutate(term_doc_count = 1) %>%
    group_by(term) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()

  bow <- bow %>%
    left_join(documentcount, by = "document") %>%
    left_join(termcount, by = "term") %>%
    bind_tf_idf(term, document, count)

  return(bow)
}
