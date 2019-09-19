#' Transform the bags of words into a document to term matrix/
#' @param bow       Tibble. Output of the function text_bow_metrics. Document ids must be in a variable called "document".
#' @param min_term  Integer. Remove terms appearing less than this number of times.
#' @param max_term  Integer. Remove terms appearing more than this number of times.
#' @param min_doc   Integer, Remove terms appearing in less than this number of documents.
#' @param max_doc   Integer, Remove terms appearing in more than this number of documents.
#' @param nbterm    Integer. Select this number of terms based on tf idf.
#' @param docvar    Additional information about documents to be appended to the docvar of the dtm. Document ids must be in a variable called "document".
#' @return A document to term matrix.
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr top_n
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map_dbl
#' @export


text_bow2dtm <- function(bow,
                         min_term = 0,
                         max_term = Inf,
                         min_doc = 0,
                         max_doc = Inf,
                         nbterm = 1000,
                         docvar = NULL){
  
  term_count <- NULL
  term_doc_count <- NULL
  term <- NULL
  data <- NULL
  maxtfidf <- NULL
  doc_word_count <- NULL
  count <- NULL
  document <- NULL
  
  dtm <- bow %>%
    dplyr::filter(
      term_count >= min_term,
      term_count <= max_term,
      term_doc_count >= min_doc,
      term_doc_count <= max_doc
    ) %>%
    dplyr::group_by(term) %>%
    tidyr::nest() %>%
    dplyr::mutate(maxtfidf = purrr::map_dbl(data, function(x) max(x$tf_idf))) %>%
    dplyr::ungroup() %>%
    dplyr::top_n(nbterm, maxtfidf) %>%
    dplyr::select(-maxtfidf) %>%
    tidyr::unnest(data)
  
  doc <- unique(dplyr::select(dtm, document, word_count = doc_word_count))
  if (!is.null(docvar)){
    doc <- dplyr::left_join(doc, docvar, by = "document")
  }
  
  dtm <- dtm %>%
    dplyr::select(document, term, count) %>%
    tidytext::cast_dfm(document = document, term = term, value = count)
  
  dtm@docvars <- doc
  
  return(dtm)
}
