#' Transform the documents stored in a dataframe in a bag of words.
#' @param src   character string. id of the source text to which the list of strings is compared.
#' @param x     character string. source text to which the list of target strings is compared.
#' @param y     list od strings. list of target strings to which the source string is compared.
#' @return A tibble indicating the overlaps between strings.
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @export

text_overlap <- function(src,x,y){
  
  content <- NULL
  tgt <- NULL
  inSource <- NULL
  inTarget <- NULL
  inBoth <- NULL
  
  init <- unlist(strsplit(x, split = " "))
  
  overlap <- tibble::tibble(
    src = src,
    inSource = length(init),
    tgt = 1:length(y),
    content = y
  ) %>%
    dplyr::mutate(
      content = purrr::map(content, function(x) unlist(strsplit(x, split = " ")))
    ) 
  
  overlap <- overlap %>%
    dplyr::mutate(
      inTarget = purrr::map_dbl(content, length),
      inBoth = purrr::map_dbl(content, function(x, y) length(y[y %in% intersect(x,y)]), init)
    ) %>%
    dplyr::select(src, tgt, inSource, inTarget, inBoth)
  
  return(overlap)
}
