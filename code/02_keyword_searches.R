## Imports ----
library(tidyverse)
library(here)

## Import data ----
analytic_df <-
    readRDS(here::here("data_private", "analytic_df.RDS"))
word_df <-
    readRDS(here::here("data_private", "tokenized_words.RDS"))
sentence_df <-
    readRDS(here::here("data_private", "tokenized_sentences.RDS"))
ngram2_df <-
    readRDS(here::here("data_private", "tokenized_bigrams.RDS"))
ngram3_df <-
    readRDS(here::here("data_private", "tokenized_trigrams.RDS"))

## Helper functions ----
reshape_criterion <- function(analytic_df,
                              rule_name,
                              institutions_with_rule) {
    dplyr::tibble(institution_id = sort(unique(analytic_df$random_id)),
                  criterion = rule_name,
    ) %>%
        dplyr::mutate(value = 1 * institution_id %in% institutions_with_rule)
}

## Statement evaluations ----
##  1. Use of victims’ names
##  2. Reference to the Black community specifically
##  3. Reference to the role of the police
##  4. Specifies the act resulting in Floyd's death
##  5. Explicitly naming racism
##  6. Active support against racism
##  7. Reference to negative sequelae resulting from racism
##  8. Use of hopeful language

## 1. Use of victims’ names ----
x_george_floyd <- ngram2_df %>%
    dplyr::filter(grepl("\\<george floyd", ngram2)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_breonna_taylor <- ngram2_df %>%
    dplyr::filter(grepl("\\<breonna taylor", ngram2)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_ahmaud_arbery <- ngram2_df %>%
    dplyr::filter(grepl("\\<ahmaud arbery", ngram2)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_george_floyd <-
    reshape_criterion(analytic_df, "george_floyd", x_george_floyd) %>%
    dplyr::mutate(category_type = "sub")

mention_breonna_taylor <-
    reshape_criterion(analytic_df, "breonna_taylor", x_breonna_taylor) %>%
    dplyr::mutate(category_type = "sub")

mention_ahmaud_arbery <-
    reshape_criterion(analytic_df, "ahmaud_arbery", x_ahmaud_arbery) %>%
    dplyr::mutate(category_type = "sub")

mention_victims_names <- dplyr::bind_rows(
    mention_george_floyd,
    mention_breonna_taylor,
    mention_ahmaud_arbery,
    reshape_criterion(
        analytic_df,
        "names_victims",
        c(x_george_floyd, x_breonna_taylor, x_ahmaud_arbery)
    ) %>%
        dplyr::mutate(category_type = "main")
) %>%
    dplyr::mutate(category = "names_victims")

##  2. Reference to the Black community specifically ----
x_black <- word_df %>%
    dplyr::filter(grepl("\\<black", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_african_american <- ngram2_df %>%
    dplyr::filter(grepl("\\<african american", ngram2)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_black_community <- dplyr::bind_rows(
    reshape_criterion(analytic_df, "black", x_black) %>%
        dplyr::mutate(category_type = "sub"),
    reshape_criterion(analytic_df, "african_american", x_african_american) %>%
        dplyr::mutate(category_type = "sub"),
    reshape_criterion(
        analytic_df,
        "mentions_black_community",
        c(x_black, x_african_american)
    ) %>%
        dplyr::mutate(category_type = "main")
) %>%
    dplyr::mutate(category = "mentions_black_community")

##  3. Reference to the role of the police ----
x_police <- word_df %>%
    dplyr::filter(grepl("\\<police", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_law_enforcement <- ngram2_df %>%
    dplyr::filter(ngram2 == "law enforcement") %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_police_officer <- ngram2_df %>%
    dplyr::filter(grepl("\\<police officer", ngram2)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_police <-
    reshape_criterion(analytic_df, "police", x_police) %>%
    dplyr::mutate(category_type = "sub")

mention_police_officer <-
    reshape_criterion(analytic_df, "police_officer", x_police_officer) %>%
    dplyr::mutate(category_type = "sub")

mention_law_enforcement <-
    reshape_criterion(analytic_df, "law_enforcement", x_law_enforcement) %>%
    dplyr::mutate(category_type = "sub")

role_of_police <-
    dplyr::bind_rows(
        mention_police,
        mention_police_officer,
        mention_law_enforcement,
        reshape_criterion(
            analytic_df,
            "role_of_police",
            c(x_police, x_law_enforcement, x_police_officer)
        ) %>%
            dplyr::mutate(category_type = "main")
    ) %>%
    dplyr::mutate(category = "role_of_police")

##  4. Specifies the act resulting in Floyd's death ----
x_murder <- word_df %>%
    dplyr::filter(grepl("\\<murder", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_kill <- word_df %>%
    dplyr::filter(grepl("\\<kill", word)) %>%
    pull(random_id) %>%
    unique() %>%
    sort()

x_suffocate <- word_df %>%
    filter(grepl("\\<suffoca", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_kneeling_neck <- sentence_df %>%
    dplyr::filter(grepl("\\sknee|\\sknelt", sentence),
                  grepl("\\sneck", sentence)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_murder <-
    reshape_criterion(analytic_df, "murder", x_murder) %>%
    dplyr::mutate(category_type = "sub")

mention_kill <-
    reshape_criterion(analytic_df, "killing", x_kill) %>%
    dplyr::mutate(category_type = "sub")

mention_suffocate <-
    reshape_criterion(analytic_df, "suffocating", x_suffocate) %>%
    dplyr::mutate(category_type = "sub")

mention_kneeling_neck <-
    reshape_criterion(analytic_df, "kneeling_on_neck", x_kneeling_neck) %>%
    dplyr::mutate(category_type = "sub")

describes_the_act <-
    dplyr::bind_rows(
        mention_murder,
        mention_kill,
        mention_suffocate,
        mention_kneeling_neck,
        reshape_criterion(
            analytic_df,
            "describes_the_act",
            c(x_murder, x_kill, x_suffocate, x_kneeling_neck)
        ) %>%
            dplyr::mutate(category_type = "main")
    ) %>%
    dplyr::mutate(category = "describes_the_act")

##  5. Explicitly naming racism ----
x_racist <- word_df %>%
    dplyr::filter(grepl("\\<racist", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_racism <- word_df %>%
    dplyr::filter(grepl("\\<racism", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_white_supremacy <- ngram2_df %>%
    dplyr::filter(grepl("\\<white suprem", ngram2)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_racist <-
    reshape_criterion(analytic_df, "racist", x_racist) %>%
    dplyr::mutate(category_type = "sub")

mention_racism <-
    reshape_criterion(analytic_df, "racism", x_racism) %>%
    dplyr::mutate(category_type = "sub")

mention_white_supremacy <-
    reshape_criterion(analytic_df, "white_supremacy", x_white_supremacy) %>%
    dplyr::mutate(category_type = "sub")

role_of_racism <-
    dplyr::bind_rows(
        mention_racist,
        mention_racism,
        mention_white_supremacy,
        reshape_criterion(
            analytic_df,
            "role_of_racism",
            c(x_racist, x_racism, x_white_supremacy)
        ) %>%
            dplyr::mutate(category_type = "main")
    ) %>%
    dplyr::mutate(category = "role_of_racism")

##  6. Active support against racism ----
antiracist1 <- word_df %>%
    dplyr::filter(grepl("\\<antiraci|\\<anti-raci", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

antiracist2 <- ngram2_df %>%
    dplyr::filter(grepl("\\<anti raci|\\<anti-raci", ngram2)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_antiracism <- sort(unique(c(antiracist1, antiracist2)))

x_blm <- word_df %>%
    dplyr::filter(grepl("\\<blm|\\<blacklivesmatter", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_blm2 <- ngram3_df %>%
    dplyr::filter(grepl("black lives matter*", ngram3)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_blm3 <- sentence_df %>%
    dplyr::filter(grepl("black", sentence) &
                      grepl("lives", sentence) &
                      grepl("matter", sentence)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_antiracist <-
    reshape_criterion(analytic_df, "antiracism", x_antiracism) %>%
    dplyr::mutate(category_type = "sub")

mention_blm <-
    reshape_criterion(analytic_df, "black_lives_matter", c(x_blm, x_blm2)) %>%
    dplyr::mutate(category_type = "sub")

mention_active_support <- dplyr::bind_rows(
    mention_antiracist,
    mention_blm,
    reshape_criterion(analytic_df, "active_support", c(x_antiracism, x_blm, x_blm2)) %>%
        dplyr::mutate(category_type = "main")
) %>%
    dplyr::mutate(category = "active_support")

##  7. Reference to negative sequelae resulting from racism -----
x_inequal <- word_df %>%
    dplyr::filter(grepl("\\<inequal", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_injustice <- word_df %>%
    dplyr::filter(grepl("\\<injustice", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_inequity <- word_df %>%
    dplyr::filter(grepl("\\<inequit", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

x_disparity <- word_df %>%
    dplyr::filter(grepl("\\<disparit", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_inequal <-
    reshape_criterion(analytic_df, "inequality", x_inequal) %>%
    dplyr::mutate(category_type = "sub")

mention_injustice <-
    reshape_criterion(analytic_df, "injustice", x_injustice) %>%
    dplyr::mutate(category_type = "sub")

mention_inequity <-
    reshape_criterion(analytic_df, "inequity", x_inequity) %>%
    dplyr::mutate(category_type = "sub")

mention_disparity <-
    reshape_criterion(analytic_df, "disparity", x_disparity) %>%
    dplyr::mutate(category_type = "sub")

mention_enemy_words <- dplyr::bind_rows(
    mention_inequal,
    mention_injustice,
    mention_inequity,
    mention_disparity,
    reshape_criterion(
        analytic_df,
        "enemy_words",
        c(x_inequal, x_injustice, x_inequity, x_disparity)
    ) %>%
        dplyr::mutate(category_type = "main")
) %>%
    dplyr::mutate(category = "enemy_words")

##  8. Use of hopeful language ----
x_community <- word_df %>%
    dplyr::filter(grepl("\\<communit", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_community <-
    reshape_criterion(analytic_df, "community", x_community) %>%
    dplyr::mutate(category_type = "sub")

x_inclusion <- word_df %>%
    dplyr::filter(grepl("\\<inclus", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_inclusion <-
    reshape_criterion(analytic_df, "inclusion", x_inclusion) %>%
    dplyr::mutate(category_type = "sub")

x_justice <- word_df %>%
    dplyr::filter(grepl("\\<justice", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_justice <-
    reshape_criterion(analytic_df, "justice", x_justice) %>%
    dplyr::mutate(category_type = "sub")

x_diversity <- word_df %>%
    dplyr::filter(grepl("\\<divers", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_diversity <-
    reshape_criterion(analytic_df, "diversity", x_diversity) %>%
    dplyr::mutate(category_type = "sub")

x_solidarity <- word_df %>%
    dplyr::filter(grepl("\\<solidarity", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_solidarity <-
    reshape_criterion(analytic_df, "solidarity", x_solidarity) %>%
    dplyr::mutate(category_type = "sub")

x_equality <- word_df %>%
    dplyr::filter(grepl("\\<equal", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_equality <-
    reshape_criterion(analytic_df, "equality", x_equality) %>%
    dplyr::mutate(category_type = "sub")

x_equity <- word_df %>%
    dplyr::filter(grepl("\\<equit", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_equity <-
    reshape_criterion(analytic_df, "equity", x_equity) %>%
    dplyr::mutate(category_type = "sub")

x_support <- word_df %>%
    dplyr::filter(grepl("\\<support", word)) %>%
    dplyr::pull(random_id) %>%
    unique() %>%
    sort()

mention_support <-
    reshape_criterion(analytic_df, "support", x_support) %>%
    dplyr::mutate(category_type = "sub")

mention_hopeful_words <- dplyr::bind_rows(
    mention_community,
    mention_inclusion,
    mention_justice,
    mention_diversity,
    mention_solidarity,
    mention_equality,
    mention_equity,
    mention_support,
    reshape_criterion(
        analytic_df,
        "hopeful_words",
        c(
            x_support,
            x_equity,
            x_equality,
            x_solidarity,
            x_diversity,
            x_justice,
            x_inclusion,
            x_community
        )
    ) %>%
        dplyr::mutate(category_type = "main")
) %>%
    dplyr::mutate(category = "hopeful_words")

summary_df <- dplyr::bind_rows(
    mention_victims_names,
    mention_black_community,
    role_of_police,
    describes_the_act,
    role_of_racism,
    mention_active_support,
    mention_enemy_words,
    mention_hopeful_words
)

summary_df <- summary_df %>%
    dplyr::mutate(criterion_cat = forcats::fct_rev(factor(
        criterion,
        levels = c(
            "names_victims",
            "george_floyd",
            "breonna_taylor",
            "ahmaud_arbery",
            "mentions_black_community",
            "black",
            "african_american",
            "role_of_police",
            "police",
            "police_officer",
            "law_enforcement",
            "describes_the_act",
            "murder",
            "killing",
            "suffocating",
            "kneeling_on_neck",
            "role_of_racism",
            "racist",
            "racism",
            "white_supremacy",
            "active_support",
            "antiracism",
            "black_lives_matter",
            "enemy_words",
            "inequality",
            "injustice",
            "inequity",
            "disparity",
            "hopeful_words",
            "community",
            "inclusion",
            "justice",
            "diversity",
            "solidarity",
            "equality",
            "equity",
            "support"
        ),
        labels = c(
            "Use of victims' names",
            '    "George Floyd"    ',
            '    "Breonna Taylor"    ',
            '    "Ahmaud Arbery"    ',
            "Reference to Black people",
            '    "Black"    ',
            '    "African-American"    ',
            "Reference to the police",
            '    "Police"    ',
            '    "Police officer"    ',
            '    "Law enforcement"    ',
            "Specifies the act",
            '    "Murder"    ',
            '    "Killing"    ',
            '    "Suffocating"    ',
            '    "Kneeling on neck"    ',
            'Explicitly names racism',
            '    "Racist"    ',
            '    "Racism"    ',
            '    "White supremacy"    ',
            'Active support',
            '    "Antiracism"    ',
            '    "Black Lives Matter"    ',
            "Negative sequelae",
            '    "Inequality"    ',
            '    "Injustice"    ',
            '    "Inequity"    ',
            '    "Disparity"    ',
            "Hopeful language",
            '    "Community"    ',
            '    "Inclusion"    ',
            '    "Justice"    ',
            '    "Diversity"    ',
            '    "Solidarity"    ',
            '    "Equality"    ',
            '    "Equity"    ',
            '    "Support"    '
        ),
        ordered = TRUE
    )))

institution_ranks <- summary_df %>%
    dplyr::group_by(institution_id) %>%
    dplyr::summarize(n_true = sum(value),
                     p_true = mean(value)) %>%
    dplyr::mutate(p_rank = dplyr::row_number(dplyr::desc(p_true)))

summary_df <- summary_df %>%
    dplyr::left_join(institution_ranks)

## Save ----
saveRDS(summary_df,
        here::here("data_private", "keyword_summary_df.RDS"))
readr::write_csv(summary_df, here::here("data", "keyword_results.csv"))
