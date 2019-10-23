#' Extract bags of words from semating relationships.
#' @param semrel   Tibble. Semantic for one document relationships as returned by text_txt2semrel.
#' @param basis    Character. Whether a bag of "word", a bag of "lemma", or a bag of "stem" should be returned.
#' @param keep_pos Character vector. Parts of speech which should be kept to build the bags of words
#' @return A tibble simplified into a bag of words.
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom tibble is_tibble
#' @export

text_semrel2bow <- function(semrel,
                            basis = "lemma",
                            keep_pos = c("NOUN", "PROPN", "ADJ", "VERB", "ADV")){
  
  stopifnot(
    tibble::is_tibble(semrel),
    basis %in% c("word","lemma","stem"),
    is.character(keep_pos)
  )
  
  src_word <- NULL
  src_lemma <- NULL
  src_stem <- NULL
  src_pos <- NULL
  src_token <- NULL
  term <- NULL
  word <- NULL
  lemma <- NULL
  stem <- NULL
  
  semrel %>%
    dplyr::rename(word = src_word, lemma = src_lemma, stem = src_stem) %>%
    dplyr::mutate(
      word = tolower(word),
      lemma = tolower(lemma),
      stem = tolower(stem)
    ) %>%
    dplyr::filter(src_pos %in% keep_pos) %>%
    dplyr::select(src_token, term = basis) %>%
    unique() %>%
    dplyr::select(-src_token) %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::ungroup()
}
