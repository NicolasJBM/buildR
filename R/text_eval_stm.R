#' Return various metrics to evaluate topic model performance for different topic numbers.
#' @param dtm           dtm.
#' @param topic_nbr     Numeric.
#' @param prevalence    Formula
#' @param content       Formula
#' @param init.type     Character.
#' @param seed          Numeric
#' @param keywords      Tibble.
#' @param topic_per_doc Integer.
#' @param terms_per_topic Integer.
#' @return A tibble with various metrics of tm quality for each possible combination of the topic number and seed
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr summarise_all
#' @importFrom dplyr sample_n
#' @importFrom dplyr case_when
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr top_n
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tm stemDocument
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_int
#' @importFrom purrr pmap
#' @importFrom tibble as_tibble
#' @importFrom stm stm
#' @importFrom stm exclusivity
#' @importFrom stm checkResiduals
#' @importFrom stm semanticCoherence
#' @importFrom stm make.heldout
#' @importFrom stm eval.heldout
#' @importFrom tidytext tidy
#' @importFrom stats runif
#' @export

text_eval_stm <- function(
  dtm = NA,
  topic_nbr = 0,
  prevalence = NULL,
  content = NULL,
  init.type = "Spectral",
  seed = 1,
  keywords = NULL,
  topic_per_doc = 5,
  terms_per_topic = 5
  ){
  
  bound <- NULL
  lfact <- NULL
  document <- NULL
  topic <- NULL
  term <- NULL
  data <- NULL
  terms <- NULL
  intersection <- NULL
  
  heldout <- stm::make.heldout(dtm)
  
  model <- stm::stm(
    dtm,
    K = topic_nbr,
    prevalence = prevalence,
    content = content,
    init.type = init.type,
    seed = seed,
    verbose = FALSE
  )
  
  stm_quality <- tibble::tibble(
    exclusivity = mean(stm::exclusivity(model), na.rm = TRUE),
    semantic_coherence = mean(stm::semanticCoherence(model, documents = dtm), na.rm = TRUE),
    residuals = stm::checkResiduals(model, dtm)$dispersion,
    held_out_likelihood = stm::eval.heldout(model, heldout$missing)$expected.heldout,
    bound = unlist(max(model$convergence$bound)),
    lfact = unlist(factorial(model$settings$dim$K)),
    iter = length(model$convergence$bound)
  ) %>%
    dplyr::mutate(
      lbound = bound + lfact,
      label = list(stm::labelTopics(model, n = terms_per_topic))
    )
  
  rm(heldout)
  
  if (!is.null(keywords)){
    
    
    topic_terms <- tidytext::tidy(model, matrix = "beta") %>%
      dplyr::group_by(topic) %>%
      dplyr::top_n(terms_per_topic, beta) %>%
      dplyr::ungroup()
    
    
    
    ref_topics <- tidytext::tidy(model, matrix = "gamma", document_names = rownames(dtm)) %>%
      dplyr::group_by(document) %>%
      dplyr::top_n(topic_per_doc, gamma) %>%
      dplyr::ungroup()
    
    overlap <- ref_topics %>%
      dplyr::left_join(topic_terms, by = "topic") %>%
      dplyr::select(document, term) %>%
      dplyr::group_by(document) %>%
      tidyr::nest() %>%
      dplyr::left_join(keywords, by = "document")
    
    rm(ref_topics, topic_terms, keywords)
    gc()
      
    overlap <- overlap %>%
      dplyr::mutate(data = purrr::map(data, unlist)) %>%
      dplyr::rename(terms = data) %>%
      dplyr::mutate(terms = purrr::map(terms, tm::stemDocument)) %>%
      dplyr::mutate(terms = purrr::map(terms, unique)) %>%
      dplyr::mutate(keywords = purrr::map(keywords, function(x) if (!is.null(x)) tm::stemDocument(x) else NA)) %>%
      dplyr::mutate(keywords = purrr::map(keywords, unique)) %>%
      dplyr::mutate(
        intersection = purrr::map2_dbl(terms, keywords, function(x,y) length(intersect(x,y))),
        union = purrr::map2_dbl(terms, keywords, function(x,y) length(union(x,y))),
        term_not_key = purrr::map2_dbl(terms, keywords, function(x,y) length(setdiff(x,y))),
        key_not_term = purrr::map2_dbl(keywords, terms, function(x,y) length(setdiff(x,y)))
      ) %>%
      dplyr::mutate(
        terms = purrr::map_int(terms, length),
        keywords = purrr::map_int(keywords, length),
      ) %>%
      dplyr::mutate(
        jaccard = intersection / union,
        intensity = intersection / terms,
        coverage = intersection / keywords
      ) %>%
      dplyr::select(-document) %>%
      dplyr::summarise_all(mean)
    
    stm_quality <- bind_cols(stm_quality, overlap)
    rm(overlap)
  }
  
  return(stm_quality)
}
