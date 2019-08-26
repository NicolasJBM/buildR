#' Transform the documents stored in a dataframe in a bag of words.
#' @param x            character. Content to transform in a bag of words.
#' @param language     character string. Language for the stopwords to remove.
#' @param rm_words     character vector. List of words (or n-tokens) to remove.
#' @param min_nchar    integer. Number of characters below which the word is removed.
#' @param match_term   dataframe. Table with two variables: "word" for the string in the content and "term" for its categorization. Match words to force desired completions.
#' @return A tibble with the id of the document, words, stems, lemmas, terms, counts, and proportions.
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
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tm stopwords
#' @importFrom tm removeNumbers
#' @importFrom tm removePunctuation
#' @importFrom tm removeWords
#' @importFrom tm stripWhitespace
#' @importFrom tm stemDocument
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom purrr map
#' @importFrom stats median
#' @export

bow_from_doclist <- function(x,
                             language = "english",
                             rm_words = NULL,
                             min_nchar = 3,
                             match_term = NULL) {
  stopifnot(is.character(language), is.numeric(min_nchar))

  # bind variables
  stem <- NULL
  Freq <- NULL
  term <- NULL
  char_nbr <- NULL
  lemma <- NULL
  word <- NULL
  count <- NULL
  
  
  bow <- as.character(x) %>%
    str_replace_all(pattern = "-", replacement = " ") %>%
    str_replace_all(pattern = "[^[:alnum:]]", replacement = " ") %>%
    tolower() %>%
    removeNumbers() %>%
    removePunctuation() %>%
    removeWords(words = stopwords(language)) %>%
    stripWhitespace()

  # Remove specified additional words
  if (!is.null(rm_words)) bow <- removeWords(bow, words = rm_words)

  # Tidy the data
  bow <- bow %>%
    stripWhitespace() %>%
    str_split(pattern = " ") %>%
    unlist() %>%
    table() %>%
    as.data.frame() %>%
    rename(word = ".", count = Freq) %>%
    mutate(word = as.character(word), count = as.integer(count)) %>%
    unnest() %>%
    ungroup() %>%
    filter(nchar(word) >= min_nchar) %>%
    mutate(stem = stemDocument(word))

  # Match words with desired terms
  if (!is.null(match_term)) {
    bow <- bow %>% left_join(match_term, by = "word")
  } else {
    bow <- bow %>% mutate(term = NA) %>% mutate(term = as.character(term))
  }

  # Prepare automatic completion for the rest
  for_completion <- bow %>%
    select(word, stem, count) %>%
    group_by(word, stem) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup() %>%
    group_by(stem) %>%
    filter(count >= median(count)) %>%
    mutate(char_nbr = nchar(word)) %>%
    filter(char_nbr == min(char_nbr)) %>%
    sample_n(1) %>%
    ungroup() %>%
    select(stem, term2 = word)

  bow <- bow %>%
    left_join(for_completion, by = "stem") %>%
    mutate(
      term = case_when(
        !is.na(term) ~ term,
        TRUE ~ term2
      )
    ) %>%
    select(word, stem, lemma, term, count)

  return(bow)
}
