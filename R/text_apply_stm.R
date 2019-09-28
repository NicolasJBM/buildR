#' Apply the topic mode to a new corpus of documents.
#' @param bow       Tibble. Bag of words as returned by the function text_semrel2bow.
#' @param stm       stm. Structural topic model as returned by the function stm.
#' @param labels    Tibble. One variable for "topic", one variable for "label".
#' @param keywords  Tibble. One variable specifying the "document", one variable containing the "keywords" as a character vector.
#' @param min_gamma Numeric. Minumum strength of the relationship between topic and socument.
#' @param min_common Integer. Maximum number of topics allowed per document.
#' @return A tibble linking topics and documents for a new corpus.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom stm textProcessor
#' @importFrom stm prepDocuments
#' @importFrom stm alignCorpus
#' @importFrom stm fitNewDocuments
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom stringr str_remove_all
#' @export


text_apply_stm <- function(bow,
                           stm,
                           labels = NULL,
                           keywords = NULL,
                           min_gamma = 0.1,
                           min_common = 1){
  
  term <- NULL
  document <- NULL
  topic <- NULL
  label <- NULL
  nbkw <- NULL
  common <- NULL
  
  doc <- bow %>%
    dplyr::mutate(term = purrr::map2(term, count, function(x,y) paste(rep(x,y), collapse = " "))) %>%
    dplyr::select(document, term) %>%
    dplyr::group_by(document) %>%
    dplyr::summarise(term = paste(term, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(document)
  
  newdoc <- stm::textProcessor(doc$term, removestopwords = FALSE, removenumbers = FALSE, removepunctuation = FALSE, stem = FALSE)
  prepdoc <- stm::prepDocuments(newdoc$documents, newdoc$vocab, newdoc$meta)
  old_vocab <- stm$vocab
  
  aligneddoc <- stm::alignCorpus(prepdoc, old.vocab = old_vocab)
  
  apply_tm <- stm::fitNewDocuments(model = stm, documents = aligneddoc$documents, newData=aligneddoc$meta)
  
  prediction <- as.data.frame(apply_tm$theta)
  
  if (length(row.names(prediction)) == length(doc$document)){
    row.names(prediction) <- doc$document
  } else  {
    row.names(prediction) <- doc$document[-aligneddoc$docs.removed]
  }
  
  prediction <- prediction %>%
    tibble::rownames_to_column("document") %>%
    tidyr::gather(topic, gamma, -document) %>%
    dplyr::mutate(topic = as.integer(stringr::str_remove_all(topic, "V"))) %>%
    dplyr::filter(gamma > min_gamma)
  
  if (!is.null(keywords) & !is.null(labels)){
    
    prediction <- prediction %>%
      dplyr::left_join(labels, by = "topic") %>%
      dplyr::mutate(label = purrr::map(label, function(x) unlist(strsplit(x, split = " ")))) %>%
      dplyr::left_join(keywords, by = "document") %>%
      dplyr::mutate(common = purrr::map2_dbl(keywords, label, function(x,y) length(intersect(x, y)))) %>%
      dplyr::mutate(nbkw = purrr::map(keywords, function(x) length(unique(x)))) %>%
      dplyr::select(-label, -keywords) %>%
      dplyr::filter(nbkw <= 2 | common >= min_common)
    
  }
  
  return(prediction)
}
