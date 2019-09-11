#' Transform the documents stored in a dataframe in a bag of words.
#' @param x   character vector in which paragraphs are over multiple lines.
#' @return A character vector in which paragraphs (ending with . and with an empty line after) are recomposed.
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom dplyr select
#' @importFrom tidyr replace_na
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom purrr map
#' @export

text_unbreak <- function(x){
  
  x2 <- NULL
  endparagraph <- NULL
  paragraph <- NULL
  
  y <- tibble::tibble(x = x) %>%
    dplyr::mutate(x2 = dplyr::lead(x)) %>%
    dplyr::mutate(endparagraph = endsWith(x,".") & x2 == "") %>%
    dplyr::mutate(paragraph = cumsum(endparagraph)) %>%
    dplyr::select(-x2, -endparagraph) %>%
    dplyr::mutate(paragraph = dplyr::lag(paragraph)) %>%
    tidyr::replace_na(list(paragraph = 0)) %>%
    dplyr::group_by(paragraph) %>%
    dplyr::summarise(x = paste(x, collapse = " ")) %>%
    dplyr::select(x) %>%
    dplyr::mutate(x = purrr::map(x, trimws))
  
  unlist(y$x)
}
