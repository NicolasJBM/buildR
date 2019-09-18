#' Apply an udpipe model for part-of-speech tagging.
#' @param document character. ID of the document.
#' @param text     character. String to be parsed.
#' @param model    character. path from the working directory to the downloaded udpipe model.
#' @return A dataframe where each relationship between word is an observation
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tidyr unite
#' @importFrom udpipe udpipe_load_model
#' @importFrom udpipe udpipe
#' @export


text_parsedep <- function(document, text, model){
  
  doc_id <- NULL
  paragraph_id <- NULL
  sentence_id <- NULL
  token_id <- NULL
  token <- NULL
  src_token <- NULL
  tgt_token <- NULL
  relation <- NULL
  lemma <- NULL
  upos <- NULL
  dep_rel <- NULL
  head_token_id <- NULL
  src_token <- NULL
  src_word <- NULL
  src_lemma <- NULL
  src_pos <- NULL
  
  ud_model <- udpipe::udpipe_load_model(model)
  y <- tibble(doc_id = document, text = text) %>%
    udpipe::udpipe(ud_model, parallel.cores = 1) %>%
    tidyr::unite(src_token, doc_id, paragraph_id, sentence_id, token_id, sep = "_", remove = FALSE) %>%
    tidyr::unite(tgt_token, doc_id, paragraph_id, sentence_id, head_token_id, sep = "_", remove = TRUE) %>%
    dplyr::select(
      src_token,
      src_word = token,
      src_lemma = lemma,
      src_pos = upos,
      relation = dep_rel,
      tgt_token
    )
  
  z <- dplyr::select(y, tgt_token = src_token, tgt_word = src_word, tgt_lemma = src_lemma, tgt_pos = src_pos) %>%
    unique()
  
  y <- y %>%
    dplyr::left_join(z, by = "tgt_token")
  
  return(y)
}


