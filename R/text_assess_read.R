#' Assess the readability of a text using various indicators.
#' @param text character. Text.
#' @return Various readability scores as one observation in a tibble.
#' @import dplyr
#' @import koRpus.lang.en
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom koRpus tokenize
#' @importFrom koRpus readability
#' @importFrom purrr map_dbl
#' @export

text_assess_read <- function(text){
  tibble::tibble(text = text) %>%
    dplyr::mutate(
      text = purrr::map(text, koRpus::tokenize, format = "obj", lang = "en")
    ) %>%
    dplyr::mutate(
      text = purrr::map(
        text,
        koRpus::readability,
        index = c(
          "ARI",
          "FOG",
          "Coleman.Liau",
          "Danielson.Bryan",
          "Dickes.Steiwer",
          "ELF",
          "Flesch",
          "Flesch.Kincaid",
          "Farr.Jenkins.Paterson",
          "Fucks",
          "FORCAST",
          "Linsear.Write",
          "LIX",
          "RIX",
          "SMOG",
          "Strain",
          "Wheeler.Smith"
          ),
        quiet = TRUE)
    ) %>%
    dplyr::mutate(
      ARI = purrr::map_dbl(text, function(x) x@ARI$grade),
      FOG = purrr::map_dbl(text, function(x) x@FOG$FOG),
      Coleman.Liau = purrr::map_dbl(text, function(x) x@Coleman.Liau$grade),
      Danielson.Bryan = purrr::map_dbl(text, function(x) x@Danielson.Bryan$DB2.grade.min),
      Dickes.Steiwer = purrr::map_dbl(text, function(x) x@Dickes.Steiwer$Dickes.Steiwer),
      ELF = purrr::map_dbl(text, function(x) x@ELF$ELF),
      Flesch = purrr::map_dbl(text, function(x) x@Flesch$grade.min),
      Flesch.Kincaid = purrr::map_dbl(text, function(x) x@Flesch.Kincaid$grade),
      Farr.Jenkins.Paterson = purrr::map_dbl(text, function(x) x@Farr.Jenkins.Paterson$grade.min),
      Fucks = purrr::map_dbl(text, function(x) x@Fucks$grade),
      FORCAST = purrr::map_dbl(text, function(x) x@FORCAST$grade),
      Linsear.Write = purrr::map_dbl(text, function(x) x@Linsear.Write$grade),
      LIX = purrr::map_dbl(text, function(x) x@LIX$grade.min),
      RIX = purrr::map_dbl(text, function(x) x@RIX$grade.min),
      SMOG = purrr::map_dbl(text, function(x) x@SMOG$grade),
      Strain = purrr::map_dbl(text, function(x) x@Strain$index),
      Wheeler.Smith = purrr::map_dbl(text, function(x) x@Wheeler.Smith$score)
    ) %>%
    dplyr::select(-text)
}
