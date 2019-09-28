#' Return various metrics to evaluate topic model performance for different topic numbers.
#' @param dtm           dtm.
#' @param topic_nbr     Numeric.
#' @param prevalence    Formula
#' @param content       Formula
#' @param init.type     Character.
#' @param seed          Numeric
#' @param keywords      Tibble.
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
#' @importFrom dplyr summarise
#' @importFrom dplyr sample_n
#' @importFrom dplyr case_when
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr top_n
#' @importFrom dplyr bind_cols
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tm stemDocument
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_int
#' @importFrom purrr map_dbl
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
  terms_per_topic = 5
  ){
  
  bound <- NULL
  lfact <- NULL
  document <- NULL
  topic <- NULL
  term <- NULL
  data <- NULL
  terms <- NULL
  common <- NULL
  nbkw <- NULL
  coverage <- NULL
  topcics <- NULL
  common_max <- NULL
  common_avg <- NULL
  coverage_max <- NULL
  coverage_avg <- NULL
  
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
      dplyr::summarise(term = list(term)) %>%
      dplyr::ungroup()
    
    keep_topics <- function(kw,trm){
      trm %>%
        dplyr::mutate(common = purrr::map_dbl(term, function(trm,kw) length(intersect(kw, trm)), kw = kw)) %>%
        dplyr::mutate(nbkw = length(unique(kw))) %>%
        dplyr::filter(nbkw >= 1, common >= 1) %>%
        dplyr::mutate(coverage = common / nbkw) %>%
        dplyr::select(topic, common, coverage) %>%
        unique() %>%
        na.omit() %>%
        dplyr::summarise(
          topcics = n(),
          common_max = sum(common),
          common_avg = mean(common),
          coverage_max = max(coverage),
          coverage_avg = mean(coverage)
        )
    }
    
    overlap <- keywords %>%
      dplyr::mutate(coverage = purrr::map(keywords, keep_topics, trm = topic_terms)) %>%
      dplyr::select(-keywords) %>%
      tidyr::unnest(coverage)
    
    
    overlap <- overlap %>%
      dplyr::ungroup() %>%
      dplyr::mutate_if(is.numeric, function(x) replace(x, !is.finite(x), NA)) %>%
      na.omit() %>%
      dplyr::summarise(
        topcics = mean(topcics),
        common_max = mean(common_max),
        common_avg = mean(common_avg),
        coverage_max = mean(coverage_max),
        coverage_avg = mean(coverage_avg)
      )
    
    stm_quality <- dplyr::bind_cols(stm_quality, overlap)
  }
  
  return(stm_quality)
}
