#' Identify linebreaks which do not seem to match an end of paragraph and recompose the paragraph.
#' @param document  Character vector. A vector of lines of text to be recomposed into paragraphs.
#' @return A character vector in which paragraphs (ending with "." and with an empty line after) are recomposed.
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom dplyr select
#' @importFrom tidyr replace_na
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom purrr map
#' @export

text_recompose <- function(document){
  
  stopifnot(
    is.character(document),
    length(document > 1)
  )
  
  document2 <- NULL
  endparagraph <- NULL
  paragraph <- NULL
  
  y <- tibble::tibble(document = document) %>%
    dplyr::mutate(document2 = dplyr::lead(document)) %>%
    dplyr::mutate(endparagraph = endsWith(document,".") & document2 == "") %>%
    dplyr::mutate(paragraph = cumsum(endparagraph)) %>%
    dplyr::select(-document2, -endparagraph) %>%
    dplyr::mutate(paragraph = dplyr::lag(paragraph)) %>%
    tidyr::replace_na(list(paragraph = 0)) %>%
    dplyr::group_by(paragraph) %>%
    dplyr::summarise(document = paste(document, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(document) %>%
    dplyr::mutate(document = purrr::map(document, trimws))
  
  unlist(y$document)
}
