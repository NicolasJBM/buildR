#' Return various metrics to evaluate topic model performance for different topic numbers.
#' @param dtm dtm object.
#' @param topic_nbr Numeric vector
#' @param prevalence Formula
#' @param content Formula
#' @param init.type Character.
#' @param seed_nbr Numeric vector
#' @param keywords Tibble.
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
  topic_nbr = c(0),
  prevalence = NULL,
  content = NULL,
  init.type = "Spectral",
  seed_nbr = 1,
  keywords = NA,
  topic_per_doc = 5,
  terms_per_topic = 5
  ){
  
  # Bind variables
  Var1 <- NULL
  Var2 <- NULL
  seed <- NULL
  model <- NULL
  heldout <- NULL
  bound <- NULL
  lfact <- NULL
  residuals <- NULL
  semantic_coherence <- NULL
  held_out_likelihood <- NULL
  document <- NULL
  topic <- NULL
  term <- NULL
  data <- NULL
  terms <- NULL
  intersection <- NULL
  term_not_key <- NULL
  key_not_term <- NULL
  terms_nbr <- NULL
  keywords_nbr <- NULL
  
  
  
  topic_nbr <- setdiff(topic_nbr, 1)
  seeds <- ceiling(runif(seed_nbr) * 1000)
  
  heldout <- stm::make.heldout(dtm)
  
  tm_quality <- expand.grid(topic_nbr, seeds) %>%
    tibble::as_tibble() %>%
    dplyr::rename(topic_nbr = Var1, seed = Var2) %>%
    dplyr::mutate(
      model = purrr::map2(
        topic_nbr,
        seed,
        function(x, y, z, u, v, w) stm::stm(
          z,
          K = x,
          prevalence = u,
          content = v,
          init.type = w,
          seed = y,
          verbose = FALSE),
        z = dtm,
        u = prevalence,
        v = content,
        w = init.type
        )
      ) %>%
    dplyr::mutate(
      topic_nbr = purrr::map_dbl(model, function(x) unlist(x$settings$dim$K)),
      exclusivity = purrr::map(model, stm::exclusivity),
      residuals = purrr::map(model, stm::checkResiduals, dtm),
      semantic_coherence = purrr::map(model, stm::semanticCoherence, documents = dtm),
      held_out_likelihood = purrr::map(model, stm::eval.heldout, heldout$missing),
      bound = purrr::map_dbl(model, function(x) unlist(max(x$convergence$bound))),
      lfact = purrr::map_dbl(model, function(x) unlist(factorial(x$settings$dim$K))),
      iter = purrr::map_dbl(model, function(x) length(x$convergence$bound))
    ) %>%
    dplyr::mutate(lbound = bound + lfact) %>%
    dplyr::mutate(
      exclusivity = purrr::map_dbl(exclusivity, mean, na.rm = TRUE),
      residuals = purrr::map_dbl(residuals, "dispersion"),
      semantic_coherence = purrr::map_dbl(semantic_coherence, mean),
      held_out_likelihood = purrr::map_dbl(held_out_likelihood, "expected.heldout"))
  
  if (length(keywords)>1){
    overlap <- tm_quality %>%
      dplyr::select(topic_nbr, seed, model) %>%
      dplyr::mutate(
        topic_per_doc = topic_per_doc,
        terms_per_topic = terms_per_topic,
        keywords = list(keywords)
      ) 
    
    assess_overlap <- function(topic_nbr,  seed, model, topic_per_doc, terms_per_topic, keywords){
      
      ref_topics <- tidytext::tidy(model, matrix = "gamma", document_names = rownames(dtm)) %>%
        dplyr::group_by(document) %>%
        dplyr::top_n(topic_per_doc, gamma)
      
      topic_terms <- tidytext::tidy(model, matrix = "beta") %>%
        dplyr::group_by(topic) %>%
        dplyr::top_n(terms_per_topic, beta)
      
      document_terms <- ref_topics %>%
        dplyr::left_join(topic_terms, by = "topic") %>%
        dplyr::select(document, topic, gamma, term) %>%
        unique() %>%
        dplyr::group_by(document, topic, gamma) %>%
        tidyr::nest() %>%
        dplyr::mutate(data = purrr::map(data, unlist)) %>%
        dplyr::rename(terms = data) %>%
        dplyr::left_join(keywords, by = "document") %>%
        dplyr::mutate(terms = purrr::map(terms, tm::stemDocument)) %>%
        dplyr::mutate(terms = purrr::map(terms, unique)) %>%
        dplyr::mutate(keywords = purrr::map(keywords, function(x) if (!is.null(x)) tm::stemDocument(x) else NA)) %>%
        dplyr::mutate(keywords = purrr::map(keywords, unique)) %>%
        dplyr::mutate(
          intersection = purrr::map2(terms, keywords, intersect),
          union = purrr::map2(terms, keywords, union),
          term_not_key = purrr::map2(terms, keywords, setdiff),
          key_not_term = purrr::map2(keywords, terms, setdiff)
        ) %>%
        dplyr::mutate(
          terms_nbr = purrr::map_int(terms, length),
          keywords_nbr = purrr::map_int(keywords, length),
          intersection = purrr::map_int(intersection, length),
          union = purrr::map_int(union, length),
          term_not_key = purrr::map_int(term_not_key, length),
          key_not_term = purrr::map_int(key_not_term, length)
        ) %>%
        dplyr::mutate(
          jaccard = intersection / union,
          intensity = intersection / terms_nbr,
          coverage = intersection / keywords_nbr
        ) %>%
        dplyr::select(-document, -gamma, -terms, -keywords) %>%
        dplyr::group_by(topic) %>%
        dplyr::summarise_all(mean) %>%
        dplyr::ungroup() %>%
        dplyr::select(-topic) %>%
        dplyr::summarise_all(mean)
    }
    
    overlap$overlap <- purrr::pmap(overlap, assess_overlap)
    
    overlap <- overlap %>%
      dplyr::select(-model, -keywords) %>%
      tidyr::unnest()
    
    tm_quality <- tm_quality %>%
      dplyr::select(-model) %>%
      dplyr::left_join(overlap, by = c("topic_nbr","seed"))
  } else tm_quality <- tm_quality %>% dplyr::select(-model)
  
  return(tm_quality)
}
