#' Apply an udpipe model for part-of-speech tagging.
#' @param x  character. String to be tagged.
#' @param model character. path from the working directory to the downloaded udpipe model.
#' @return A dataframe where each token is a lemmatized and tagged observation. Tags for parts of speech are: "NOUN", "PROPN", "ADJ", "VERB", "ADV", "AUX", "PRON", "DET", "PART", "ADP", "CCONJ", "SCONJ", "NUM", "SYM", "PUNCT", "X"
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom udpipe udpipe_load_model
#' @importFrom udpipe udpipe_annotate
#' @export

text_tag <- function(x, model){
  
  doc_id <- NULL
  paragraph_id <- NULL
  sentence_id <- NULL
  token <- NULL
  lemma <- NULL
  upos <- NULL
  token_id <- NULL
  
  ud_model <- udpipe::udpipe_load_model(model)
  x <- as.data.frame(udpipe::udpipe_annotate(ud_model, x = unlist(x))) %>%
    dplyr::select(
      document = doc_id,
      paragraph = paragraph_id,
      sentence = sentence_id,
      word = token,
      lemma,
      pos = upos,
      token = token_id
    )
  return(x)
}
