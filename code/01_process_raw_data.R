## 01_process_raw_data.R ----
##
## Takes the raw data from the Google Sheets (in csv) and creates an analytic
## data frame we can use. Outputs include analytic data frame, mapping of
## random ids to institutions, data frame by words, data frame by bigrams,
## data frame by trigrams, and data frame by sentence.

## Imports ----
library(tidyverse)
library(tidytext)
library(here)
library(janitor)
library(SnowballC)

## Constants ----
CURRENT_FILE_NAME <-"current_download.csv"
MANUAL_STOPWORDS <-
    c(
        "00",
        "19",
        "25",
        "3",
        "4",
        "aamc",
        "baltimore",
        "baptist",
        "baylor",
        "brunswick",
        "chicago",
        "college",
        "columbia",
        "cornell",
        "covid",
        "dgsom",
        "dr",
        "duke",
        "forest",
        "georgia",
        "gusom",
        "hopkins",
        "houston",
        "http",
        "irving",
        "jhm",
        "johns",
        "louisville",
        "mayo",
        "minnesota",
        "nashville",
        "northwestern",
        "ohsu",
        "philadelphia",
        "rochester",
        "southwestern",
        "stanford",
        "uchicago",
        "uf",
        "umsom",
        "university",
        "ut",
        "utah",
        "uw",
        "vumc",
        "wake",
        "www.youtube.com",
        "yale"
    )

## Read in data ----
orig_df <-
    readr::read_csv(here::here("data_private", CURRENT_FILE_NAME)) %>%
    janitor::clean_names()

set.seed(123)
analytic_df <- orig_df %>%
    dplyr::filter(keep == 1) %>%
    dplyr::select(-keep, -dplyr::ends_with("_link")) %>%
    dplyr::mutate(date = lubridate::mdy(paste0(date, "/2020"))) %>%
    dplyr::rename(nih_rank = x2018_nih_funding_rank,
                  text = statement_minus_salutation_signature_resources) %>%
    dplyr::mutate(random_id = sample(1:dplyr::n()))

## Tokenize words ----
## Mark all stop words.
word_df <- analytic_df %>%
    tidytext::unnest_tokens(word, text, drop = FALSE) %>%
    dplyr::mutate(stop_word = ifelse(word %in% tidytext::stop_words$word, TRUE, FALSE)) %>%
    dplyr::mutate(stop_word = ifelse(word %in% MANUAL_STOPWORDS, TRUE, stop_word)) %>%
    dplyr::mutate(
        word_stem = SnowballC::wordStem(word),
        stop_word = ifelse(is.na(stop_word), FALSE, stop_word),
        keep_word = !stop_word
    )

## Tokenize sentences ----
sentence_df <- analytic_df %>%
    tidytext::unnest_tokens(sentence, text, drop = FALSE, token = "sentences") %>%
    dplyr::mutate(george_floyd = grepl("[Ff]{1}loyd", sentence))

## Tokenize bigrams ----
ngram2_df <- analytic_df %>%
    tidytext::unnest_ngrams(ngram_orig, text, n = 2, drop = FALSE) %>%
    tidyr::separate(ngram_orig, c("word1", "word2"), sep = " ") %>%
    dplyr::filter(!(word1 %in% c(
        MANUAL_STOPWORDS, tidytext::stop_words$word
    )),
    !(word2 %in% c(
        MANUAL_STOPWORDS, tidytext::stop_words$word
    ))) %>%
    dplyr::mutate(word1_stem = SnowballC::wordStem(word1),
                  word2_stem = SnowballC::wordStem(word2)) %>%
    dplyr::mutate(ngram2 = paste(word1, word2),
                  ngram2_stem = paste(word1_stem, word2_stem))

## Tokenize trigrams
ngram3_df <- analytic_df %>%
    tidytext::unnest_ngrams(ngram_orig, text, n = 3, drop = FALSE) %>%
    tidyr::separate(ngram_orig, c("word1", "word2", "word3"), sep = " ") %>%
    dplyr::filter(!(word1 %in% c(
        MANUAL_STOPWORDS, tidytext::stop_words$word
    )),
    !(word2 %in% c(
        MANUAL_STOPWORDS, tidytext::stop_words$word
    )),
    !(word3 %in% c(
        MANUAL_STOPWORDS, tidytext::stop_words$word
    ))) %>%
    dplyr::mutate(
        word1_stem = SnowballC::wordStem(word1),
        word2_stem = SnowballC::wordStem(word2),
        word3_stem = SnowballC::wordStem(word3)
    ) %>%
    dplyr::mutate(
        ngram3 = paste(word1, word2, word3),
        ngram3_stem = paste(word1_stem, word2_stem, word3_stem)
    )

## Save data ----
## Save analytic df and mapping
saveRDS(analytic_df, here::here("data_private", "analytic_df.RDS"))
readr::write_csv(
    analytic_df %>%
        dplyr::select(institution, nih_rank, usnwr_rank, random_id),
    here::here("data_private", "random_id_mapping.csv")
)

## Save tokenized data
saveRDS(word_df, here::here("data_private", "tokenized_words.RDS"))
saveRDS(sentence_df,
        here::here("data_private", "tokenized_sentences.RDS"))
saveRDS(ngram2_df,
        here::here("data_private", "tokenized_bigrams.RDS"))
saveRDS(ngram3_df,
        here::here("data_private", "tokenized_trigrams.RDS"))
