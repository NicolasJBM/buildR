#' Extract bags of words from semating relationships.
#' @param semrel   Tibble. Semantic for one document relationships as returned by text_txt2semrel.
#' @param basis    Character. Whether a bag of "word" or a bag of "lemma" should be returned.
#' @param keep_pos Character vector. Parts of speech which should be kept to build the bags of words
#' @param min_count Numeric. Minimum number of occurrences for a term to be kept.
#' @return A tibble simplified into a bag of words.
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr rename
#' @importFrom tibble is_tibble
#' @export

text_semrel2bow <- function(semrel,
                            basis = "lemma",
                            keep_pos = c("NOUN", "PROPN", "ADJ", "VERB", "ADV"),
                            min_count = 1){
  
  stopifnot(
    tibble::is_tibble(semrel),
    basis %in% c("word","lemma","stem"),
    is.character(keep_pos),
    is.numeric(min_count)
  )
  
  src_word <- NULL
  src_lemma <- NULL
  src_stem <- NULL
  src_pos <- NULL
  src_token <- NULL
  term <- NULL
  
  semrel %>%
    dplyr::rename(word = src_word, lemma = src_lemma, stem = src_stem) %>%
    dplyr::filter(src_pos %in% keep_pos) %>%
    dplyr::select(src_token, term = basis) %>%
    unique() %>%
    dplyr::select(-src_token) %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::filter(count >= min_count) %>%
    dplyr::ungroup()
}
