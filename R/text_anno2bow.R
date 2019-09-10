#' Transform the documents stored in a dataframe in a bag of words.
#' @param x        tibble. Output of the function text_annotate.
#' @param base     character string. whether a bag of "word"s or a bag of "lemma"s should be produced.
#' @param keep     character vector. Which parts of speech should be keept when building the bag of words.
#' @return A tibble with the document, term, and count.
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr n
#' @importFrom stringr str_replace
#' @export

text_anno2bow <- function(x,
                          base = "lemma",
                          keep = c("NOUN", "PROPN", "ADJ", "VERB", "ADV")) {
  
  pos <- NULL
  term <- NULL
  
  stopifnot(
    base %in% c("lemma","word"),
    length(intersect(x$pos, keep)) > 0
  )
  
  names(x) <- stringr::str_replace(names(x), base, "term")
  
  x %>%
    dplyr::filter(pos %in% keep) %>%
    dplyr::select(term) %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(count = dplyr::n())
}
