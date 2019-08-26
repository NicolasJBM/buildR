#' Count occurrences of a focal regex expression (near an adjacent one if specified) in the result of a udpipe part-of-speech tagging process.
#' @param text         character. Text.
#' @param focal        regex pattern. Pattern around which the adjacent pattern has to be detected within the window.
#' @param dictionary   dataframe. 2 variables: pattern and category. Specify and classify regex patterns.
#' @param window       numeric vector. Two values: one indicating the beginning of the window from the focal pattern, and one indicating the end.
#' @return A numeric value indicating how many times the focal pattern occurred.
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr everything
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom purrr map_lgl
#' @importFrom purrr map2
#' @importFrom tidyr unnest
#' @importFrom tibble tibble
#' @importFrom stringr str_split
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_detect
#' @export


count_in_window <- function(text, focal, dictionary, window = c(3,3)){
  
  # Bind variables
  category <- NULL
  pattern <- NULL
  content <- NULL
  term <- NULL
  near_term <- NULL
  near_length <- NULL
  
  # Add function
  find_near <- function(content, focal, window = c(3,3)){
    expression <- paste0("([^\\s]+\\s){0,", window[1],"}",focal,".\\w+(\\s[^\\s]+){0,", window[2],"}")
    unlist(stringr::str_extract_all(content, expression))
  }
  
  # Prepare dictionary
  classified <- paste(dictionary$pattern, collapse = "|")
  classification <- dictionary %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(pattern = paste(pattern, collapse = "|"))
  
  clean_text <- text %>%
    str_replace_all("[^[:alnum:]]", " ") %>%
    str_replace_all("[[:punct:][:blank:]]+", " ") %>%
    tolower() %>%
    trimws()
  
  # Find matches
  x <- tibble::tibble(content = clean_text) %>%
    dplyr::mutate(near_term = purrr::map(content, find_near, focal = focal, window = window)) %>%
    dplyr::mutate(near_length = purrr::map_dbl(near_term, length)) %>%
    dplyr::filter(near_length != 0) %>%
    dplyr::select(-content, -near_length) %>%
    dplyr::mutate(near_term = purrr::map(
      near_term,
      function(x) unlist(stringr::str_split(x, " ")))
    ) %>% 
    tidyr::unnest() %>%
    dplyr::filter(
      stringr::str_detect(near_term, classified)
    )
  
  # Count
  for (i in 1:nrow(classification)){
    x[,as.character(classification$category)[i]] <- as.numeric(unlist(purrr::map(x$near_term, stringr::str_detect, classification$pattern[i])))
  }
  
  x <- x %>%
    dplyr::mutate(term = focal) %>%
    select(term, everything())
  return(x)
}


