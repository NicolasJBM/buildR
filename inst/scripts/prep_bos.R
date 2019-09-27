library(tools)
library(stringi)
library(dplyr)
library(readxl)
library(purrr)
library(tidyr)
library(stringdist)

bos_en_lemmas <- read_excel("inst/scripts/en_lemmas.xlsx")

bos_en_lemmas <- bos_en_lemmas %>%
  mutate(
    ascii1 = stringi::stri_enc_mark(word),
    ascii2 = stringi::stri_enc_mark(lemma),
    ascii3 = stringi::stri_enc_mark(term)
  ) %>%
  filter(ascii1 == "ASCII", ascii2 == "ASCII", ascii3 == "ASCII") %>%
  select(word, lemma, term) %>%
  mutate(word = iconv(word, to="ASCII//TRANSLIT"),
         lemma = iconv(lemma, to="ASCII//TRANSLIT"),
         term = iconv(term, to="ASCII//TRANSLIT")
  )

add_term <- tibble(
  word = unique(bos_en_lemmas$term)
) %>%
  mutate(lemma = word, term = word)

bos_en_lemmas <- bos_en_lemmas %>%
  bind_rows(add_term) %>%
  mutate(
    difference = stringdist(word, term),
    characters = nchar(term)
  ) %>%
  group_by(word) %>%
  nest() %>%
  mutate(data = map(data, filter, difference == min(difference))) %>%
  mutate(data = map(data, filter, characters == max(characters))) %>%
  mutate(data = map(data, sample_n, 1)) %>%
  unnest() %>%
  ungroup() %>%
  select(-characters) %>%
  unique()


save(bos_en_lemmas, file = "data/bos_en_lemmas.RData")
resaveRdaFiles("data/bos_en_lemmas.RData")


library(dplyr)

bos_toascii <- data.frame(
  mapL = c("[á]","[é]","[í]","[ó]","[ú]","[Á]","[É]","[Í]","[Ó]","[Ú]","[ñ]","[Ñ]","[ü]","[Ü]","[ç]","[ä]","[Ä]","[ë]","[Ë]","[ï]","[Ï]","[ö]","[Ö]","[ü]","[Ü]","[ÿ]","[Ÿ]","[â]","[Â]","[ê]","[Ê]","[î]","[Î]","[ô]","[Ô]","[û]","[Û]","[à]","[À]","[è]","[È]","[ì]","[Ì]","[ò]","[Ò]","[ù]","[Ù]"),
  mapA = c("a","e","i","o","u","A","E","I","O","U","n","N","u","U","c","a","A","e","E","i","I","o","O","u","U","y","Y","a","A","e","E","i","I","o","O","u","U","a","A","e","E","i","I","o","O","u","U")
)

bos_toascii <- mutate(bos_toascii, mapL = as.character(mapL), mapA = as.character(mapA))


save(bos_toascii, file = "data/bos_toascii.RData")



bos_dictionaries <- read.csv("inst/scripts/dictionaries.csv")
save(bos_dictionaries, file = "data/bos_dictionaries.RData")


bos_symbols <- "[~#\\(\\)\\\\@/%$£€&^]"
save(bos_symbols, file = "data/bos_symbols.RData")

