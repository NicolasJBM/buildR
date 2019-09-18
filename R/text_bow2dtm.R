#' Transform the bags of words into a document to term matrix/
#' @param x                  tibble. Output of the function text_bow_metrics.
#' @param document           character. name of the variable identifying documents.
#' @param min_term_count     Integer. Remove terms appearing less than this number of times.
#' @param min_term_src_count Integer, Remove terms appearing in less than this number of documents.
#' @param nbterm             Integer. Select this number of terms based on tf idf.
#' @param docvar             Additional information about documents to be appended to the docvar of the dtm.
#' @return A document to term matrix.
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr top_n
#' @importFrom dplyr left_join
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map_dbl
#' @export


text_bow2dtm <- function(x,
                         document = "document",
                         min_term_count = 0,
                         min_term_src_count = 0,
                         nbterm = 1000,
                         docvar = NULL){
  
  term_count <- NULL
  term_src_count <- NULL
  term <- NULL
  data <- NULL
  maxtfidf <- NULL
  src_word_count <- NULL
  count <- NULL
  
  dtm <- x %>%
    dplyr::filter(
      term_count > min_term_count,
      term_src_count > min_term_src_count
    ) %>%
    dplyr::group_by(term) %>%
    tidyr::nest() %>%
    dplyr::mutate(maxtfidf = purrr::map_dbl(data, function(x) max(x$tf_idf))) %>%
    dplyr::top_n(nbterm, maxtfidf) %>%
    dplyr::select(-maxtfidf) %>%
    tidyr::unnest(data)
  
  doc <- unique(dplyr::select(dtm, document = document, word_count = src_word_count))
  if (!is.null(docvar)){
    doc <- dplyr::left_join(doc, docvar, by = "document")
  }
  
  dtm <- dtm %>%
    dplyr::select(document = document, term, count) %>%
    tidytext::cast_dfm(document = document, term = term, value = count)
  
  dtm@docvars <- doc
  
  return(dtm)
}
