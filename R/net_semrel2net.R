#' Make an tidygraph from a list of semantic relationships as returned by text_txt2semrel.
#' @param semrel    Tibble. Output of the function text_txt2semrel
#' @param basis     Character. Whether the network should be based on "word" or "lemma".
#' @param keep_pos  Character vector. Parts of speech which should be kept to build the network.
#' @param multiplex Logical. Whether the kind of relationship should be kept.
#' @param min_count Numeric. Minimum number of occurrences for a relationship to be kept.
#' @return A tidygraph of semantic relationships between words of lemmas
#' @importFrom tibble is_tibble
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom dplyr n
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom tidygraph as_tbl_graph
#' @importFrom tidygraph activate
#' @importFrom stats na.omit
#' @export

net_semrel2net <- function(semrel,
                            basis = "lemma",
                            keep_pos = c("NOUN", "PROPN", "ADJ", "VERB", "ADV"),
                            multiplex = FALSE,
                            min_count = 1){
  
  stopifnot(
    tibble::is_tibble(semrel),
    basis %in% c("word","lemma"),
    is.character(keep_pos),
    is.logical(multiplex),
    is.numeric(min_count)
  )
  
  src_word <- NULL
  src_lemma <- NULL
  src_pos <- NULL
  src_token <- NULL
  term <- NULL
  src_pos <- NULL
  tgt_pos <- NULL
  relation <- NULL
  rel <- NULL
  tgt_token <- NULL
  tgt_word <- NULL
  tgt_lemma <- NULL
  name <- NULL
  
  if (basis == "word") src <- "src_word" else src <- "src_lemma"
  if (basis == "word") tgt <- "tgt_word" else tgt <- "tgt_lemma"
  
  edges <- semrel %>%
    dplyr::filter(src_pos %in% keep_pos, tgt_pos %in% keep_pos)
    
  if (multiplex){
    edges <- edges %>%
      dplyr::select(src = src, src_pos, tgt = tgt, tgt_pos, rel = relation) %>%
      dplyr::group_by(src, src_pos, tgt, tgt_pos, rel) %>%
      dplyr::summarise(weight = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(weight >= min_count) %>%
      na.omit()
  } else {
    edges <- edges %>%
      dplyr::select(src = src, tgt = tgt) %>%
      dplyr::group_by(src, tgt) %>%
      dplyr::summarise(weight = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(weight >= min_count) %>%
      na.omit()
  }
  
  nodes <- dplyr::bind_rows(
    dplyr::select(semrel, token = src_token, word = src_word, lemma = src_lemma),
    dplyr::select(semrel, token = tgt_token, word = tgt_word, lemma = tgt_lemma)
  ) %>%
    unique() %>%
    dplyr::select(name = basis) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(occurrences = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(name %in% unique(union(edges$src, edges$tgt)))
  
  if (multiplex){
    graph <- edges %>%
      dplyr::select(from = src, to = tgt, src_pos, tgt_pos, rel, count)
  } else {
    graph <- edges %>%
      dplyr::select(from = src, to = tgt, count)
  }
  
  if (nrow(edges) > 0){
    graph <- graph %>%
      tidygraph::as_tbl_graph() %>%
      tidygraph::activate("nodes") %>%
      dplyr::left_join(nodes, by = "name") %>%
      dplyr::mutate(id = 1:length(name))
  } else graph <- NA
  
  return(graph)
}
