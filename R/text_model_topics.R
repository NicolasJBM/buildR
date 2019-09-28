#' Apply the topic mode to a new corpus of documents.
#' @param dtm             dtm.
#' @param topic_nbr       Numeric.
#' @param prevalence      Formula
#' @param content         Formula
#' @param init.type       Character.
#' @param seed            Numeric
#' @param keywords        Tibble.
#' @param terms_per_topic Integer.
#' @param baselab         Character. Whether labels should be based on "prob", "frex", "lift", or "score".
#' @param frexweight      Numeric. The weight on discriminance in term selection.
#' @param min_gamma       Numeric. Minimum strength of the relationship between document and topic.
#' @param min_common      Integer. Minimum number of keywords in the label.
#' @return A list with the "topic_model", the "topic_label", and the "document_topic".
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
#' @export


text_model_topics <- function(dtm,
                              topic_nbr = 0,
                              prevalence = NULL,
                              content = NULL,
                              init.type = "Spectral",
                              seed = 1234,
                              keywords = NULL,
                              terms_per_topic = 5,
                              baselab = "prob",
                              frexweight = 0.6,
                              min_gamma =  0.1,
                              min_common = 1){
  
  label <- NULL
  topic <- NULL
  document <- NULL
  nbkw <- NULL
  common <- NULL
  
  topic_model <- stm::stm(
    dtm,
    K = topic_nbr,
    prevalence = prevalence,
    content = ,
    init.type = init.type,
    seed = seed,
    verbose = FALSE)
  
  topic_labels <- stm::labelTopics(topic_model, n = terms_per_topic, frexweight = frexweight)[[baselab]] %>%
    as.data.frame() %>%
    tidyr::unite(label, sep = " ") %>%
    tibble::rownames_to_column("topic") %>%
    dplyr::mutate(topic = as.integer(topic))
  
  document_topic <- topic_model$theta %>%
    as.data.frame() %>%
    dplyr::mutate(document = dtm@Dimnames$docs) %>%
    tidyr::gather(topic, gamma, -document) %>%
    dplyr::mutate(topic = as.integer(gsub("V", "", topic))) %>%
    dplyr::filter(gamma >= min_gamma)
  
  
  if (!is.null(keywords)){
    
    document_topic <- document_topic %>%
      dplyr::left_join(topic_labels, by = "topic") %>%
      dplyr::mutate(label = purrr::map(label, function(x) unlist(strsplit(x, split = " ")))) %>%
      dplyr::left_join(keywords, by = "document") %>%
      dplyr::mutate(common = purrr::map2_dbl(keywords, label, function(x,y) length(intersect(x, y)))) %>%
      dplyr::mutate(nbkw = purrr::map(keywords, function(x) length(unique(x)))) %>%
      dplyr::select(-label, -keywords) %>%
      dplyr::filter(nbkw <= 2 | common >= min_common)
    
  }
  
  result <- list(
    topic_model = topic_model,
    topic_labels = topic_labels,
    document_topic = document_topic
  )
  
  return(result)
}
