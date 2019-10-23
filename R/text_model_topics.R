#' Apply the topic mode to a new corpus of documents.
#' @param dtm             dtm.
#' @param topic_nbr       Numeric.
#' @param prevalence      Formula
#' @param content         Formula
#' @param init.type       Character.
#' @param seed            Numeric
#' @param keywords        Tibble.
#' @param baselab         Character. Whether labels should be based on "prob", "frex", "lift", or "score".
#' @param frexweight      Numeric. The weight on discriminance in term selection.
#' @param min_beta        Numeric. Minimum strength of the relationship between term and topic.
#' @param min_gamma       Numeric. Minimum strength of the relationship between document and topic.
#' @param min_common      Integer. Maximum number of topics allowed per document.
#' @return A list with the "topic_model", the "topic_term", the "topic_label", and the "topic_document".
#' @importFrom stm stm
#' @importFrom stm labelTopics
#' @importFrom tidyr unite
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom purrr map
#' @importFrom purrr map2_dbl
#' @importFrom dplyr select
#' @importFrom tidytext tidy
#' @export


text_model_topics <- function(dtm,
                              topic_nbr = 0,
                              prevalence = NULL,
                              content = NULL,
                              init.type = "Spectral",
                              seed = 1234,
                              keywords = NULL,
                              baselab = "prob",
                              frexweight = 0.6,
                              min_beta = 0.01,
                              min_gamma =  0.1,
                              min_common = 1){
  
  label <- NULL
  topic <- NULL
  document <- NULL
  nbkw <- NULL
  nbtrm <- NULL
  jaccard <- NULL
  common <- NULL
  term <- NULL
  
  topic_model <- stm::stm(
    dtm,
    K = topic_nbr,
    prevalence = prevalence,
    content = content,
    init.type = init.type,
    seed = seed,
    verbose = FALSE)
  
  topic_term <- tidytext::tidy(topic_model, matrix = "beta") %>%
    dplyr::group_by(term) %>%
    dplyr::top_n(1, beta) %>%
    dplyr::ungroup() %>%
    dplyr::filter(beta >= min_beta) %>%
    dplyr::group_by(topic) %>%
    dplyr::summarise(term = list(term)) %>%
    dplyr::ungroup() %>%
    dplyr::select(topic, term) %>%
    dplyr::mutate(topic = as.integer(topic))
  
  topic_label <- stm::labelTopics(topic_model, n = 4, frexweight = frexweight)[[baselab]] %>%
    as.data.frame() %>%
    tidyr::unite(label, sep = " ") %>%
    tibble::rownames_to_column("topic") %>%
    dplyr::mutate(topic = as.integer(topic))
  
  topic_document <- topic_model$theta %>%
    as.data.frame() %>%
    dplyr::mutate(document = dtm@Dimnames$docs) %>%
    tidyr::gather(topic, gamma, -document) %>%
    dplyr::mutate(topic = as.integer(gsub("V", "", topic))) %>%
    dplyr::filter(gamma >= min_gamma)
  
  
  if (!is.null(keywords)){
    
    topic_document <- topic_document %>%
      dplyr::left_join(topic_term, by = "topic") %>%
      dplyr::left_join(keywords, by = "document") %>%
      dplyr::mutate(common = purrr::map2_dbl(keywords, term, function(x,y) length(intersect(x, y)))) %>%
      dplyr::mutate(nbkw = purrr::map_dbl(keywords, function(x) length(unique(x)))) %>%
      dplyr::mutate(nbkw = case_when(is.finite(nbkw) & nbkw > 0 ~ nbkw, TRUE ~ 1)) %>%
      dplyr::mutate(nbtrm = purrr::map_dbl(term, function(x) length(unique(x)))) %>%
      dplyr::mutate(
        prop_kw = common / nbkw,
        prop_trm = common / nbtrm,
        jaccard = common / (nbkw + nbtrm - common)
      ) %>%
      dplyr::select(-keywords, -term) %>%
      dplyr::filter(nbkw <= 2 | common >= min_common)
    
  }
  
  result <- list(
    topic_model = topic_model,
    topic_term = topic_term,
    topic_label = topic_label,
    topic_document = topic_document
  )
  
  return(result)
}
