#' Return various metrics to evaluate topic model performance for different topic numbers.
#' @param dtm           dtm.
#' @param topic_nbr     Numeric.
#' @param prevalence    Formula
#' @param content       Formula
#' @param init.type     Character.
#' @param seed          Numeric
#' @param keywords      Tibble.
#' @param min_beta      Numeric. Minimum strength of the relationship between term and topic.
#' @param min_gamma     Numeric. Minimum strength of the relationship between document and topic.
#' @return A list with the "topic_model", the "topic_label", and the "document_topic".
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
  min_beta = 0.01,
  min_gamma =  0.1
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
  topics <- NULL
  nbtrm <- NULL
  prop_kw <- NULL
  prop_trm <- NULL
  jaccard <- NULL
  
  heldout <- stm::make.heldout(dtm)
  
  model <- buildR::text_model_topics(
    dtm,
    topic_nbr = topic_nbr,
    prevalence = prevalence,
    content = content,
    init.type = init.type,
    seed = seed,
    keywords = keywords,
    min_beta = min_beta,
    min_gamma = min_gamma
  )
  
  tm <- model$topic_model
  td <- model$topic_document
  
  stm_quality <- tibble::tibble(
    topics = length(unique(td$topic)),
    exclusivity = mean(stm::exclusivity(tm), na.rm = TRUE),
    semantic_coherence = mean(stm::semanticCoherence(tm, documents = dtm), na.rm = TRUE),
    residuals = stm::checkResiduals(tm, dtm)$dispersion,
    held_out_likelihood = stm::eval.heldout(tm, heldout$missing)$expected.heldout,
    bound = unlist(max(tm$convergence$bound)),
    lfact = unlist(factorial(tm$settings$dim$K)),
    iter = length(tm$convergence$bound)
  ) %>%
    dplyr::mutate(
      lbound = bound + lfact
    )
  
  rm(heldout)
  
  if (!is.null(keywords)){
    
    td <- td %>%
      select(-nbkw) %>%
      filter(common > 0) %>%
      unique() %>%
      group_by(document) %>%
      summarise(
        topic = n(),
        gamma = max(gamma),
        common = max(common),
        prop_kw = max(prop_kw),
        prop_trm = max(prop_trm),
        jaccard = max(jaccard)
      ) %>%
      select(-document) %>%
      summarise(
        document = n(),
        tpd_min = min(topic),
        tpd_avg = mean(topic),
        tpd_med = median(topic),
        tpd_max = max(topic),
        gamma_avg = mean(gamma),
        common_avg = mean(common),
        prop_kw_avg = mean(prop_kw),
        prop_trm_avg = mean(prop_trm),
        jaccard_avg = mean(jaccard)
      )
    
    stm_quality <- dplyr::bind_cols(stm_quality, td)
  }
  
  return(stm_quality)
}
