#' Make a network based on cooccurrences of themes (use the document as unit of analysis).
#' @param x          data.frame. One numeric variable per theme, document ids as row names.
#' @param dictionary data.frame. Dictionary specifying the attributes of the themes, the variabme "name" identifying each theme.
#' @return A network object
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @importFrom psych corr.test
#' @importFrom tibble rownames_to_column
#' @importFrom tidygraph as_tbl_graph
#' @importFrom tidygraph activate
#' @export


make_cooc_net <- function(x, dictionary){
  
  # bind variables
  from <- NULL
  to <- NULL
  correlation <- NULL
  cooccurrences <- NULL
  theme <- NULL
  freq_from <- NULL
  freq_to <- NULL
  frequency <- NULL
  max_freq <- NULL
  prob_from_if_to <- NULL
  prob_to_if_from <- NULL
  prob_from <- NULL
  prob_to <- NULL
  jaccard <- NULL
  sorenson <- NULL
  prob_leverage <- NULL
  nodes <- NULL
  category <- NULL
  
  corr <- corr.test(x)$r %>%
    as.data.frame() %>%
    rownames_to_column("from") %>%
    gather(to, correlation, -from)
  
  x <- mutate_all(x, function(x) as.numeric(x > 0))
  
  base_census <- nrow(x)
  theme_census <- colSums(x) %>%
    as.data.frame()
  names(theme_census) <- "frequency"
  theme_census <- theme_census %>%
    rownames_to_column("theme")
  
  xmat <- as.matrix(x)
  
  cooc <- t(xmat) %*% xmat %>%
    as.data.frame() %>%
    rownames_to_column("from") %>%
    gather(to, cooccurrences, -from) %>%
    mutate(max_freq = base_census) %>%
    left_join(select(theme_census, from = theme, freq_from = frequency), by = "from") %>%
    left_join(select(theme_census, to = theme, freq_to = frequency), by = "to") %>%
    filter(from != to) %>%
    mutate(
      jaccard = cooccurrences / (freq_from + freq_to - cooccurrences),
      sorenson = 2*cooccurrences / (freq_from + freq_to),
      prob_from = freq_from / max_freq,
      prob_to = freq_to / max_freq,
      prob_from_and_to = cooccurrences / max_freq,
      prob_from_if_to = cooccurrences / freq_to,
      prob_to_if_from = cooccurrences / freq_from
    ) %>%
    mutate(
      prob_leverage = prob_from_if_to / prob_from
    ) %>%
    left_join(corr, by = c("from","to"))
  
  network <- cooc %>%
    select(from, to, cooccurrences, correlation, jaccard, sorenson, prob_from_if_to, prob_to_if_from, prob_leverage) %>%
    as_tbl_graph() %>%
    activate(nodes) %>%
    left_join(unique(select(cooc, name = from, frequency = freq_from, maximum = max_freq)), by = "name") %>%
    left_join(unique(select(dictionary, name = theme, category)), by = "name")
  
  return(network)
}
