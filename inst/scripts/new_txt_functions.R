

txt_annotate <- function(x, model){
  ud_model <- udpipe::udpipe_load_model(model)
  x <- as.data.frame(udpipe::udpipe_annotate(ud_model, x = x)) %>%
    dplyr::select(document = doc_id, paragraph = paragraph_id, sentence = sentence_id, word = token, lemma, type = upos, token = token_id)
  return(x)
}


txt_filter <- function(x,
                       keep = c("NOUN","ADJ","PROPN",
                                "VERB","ADV",
                                "AUX",
                                "PRON","DET",
                                "ADP","PART",
                                "CCONJ","SCONJ",
                                "NUM","SYM","PUNCT",
                                "X")){
  dplyr::filter(type %in% keep)
}



txt_overlap <- function(x,id,y){
  
  x <- unique(unlist(strsplit(x, split = " ")))
  
  overlap <- tibble(
    source_id = id,
    inSource = length(x),
    target_id = 1:length(y),
    target = y
  ) %>%
  mutate(
    target = future_map(target, function(target) unique(unlist(strsplit(target, split = " "))))
  ) 
  
  overlap <- overlap %>%
  mutate(
    inSource = length(x),
    inTarget = furrr::future_map_dbl(target, length),
    inBoth = furrr::future_map_dbl(target, function(x, y) length(intersect(x,y)), x)
  ) %>%
    select(source_id, target_id, inSource, inTarget, inBoth)
  
  return(overlap)
}
