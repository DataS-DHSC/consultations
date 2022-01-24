#' Extract most common bigrams or trigrams
#'
#' For a free-text column, return the most commonly used bigrams or trigrams.
#'
#' @param df A data frame with one free-text column.
#' #' If your text is spread across several columns, you can merge them like this:
#' df <- data.frame(text_col = unlist(df %>% select(question_cols))) %>% filter(!is.na(text_col))
#' @param text_col_name A string containing the name of the free-text column.
#' @param ngram_type A string. Should be "bigrams" or "trigrams".
#' @param min_freq Integer. Number of minimum count required for a ngram to be included in output. Defaults to 1.
#'
#' @return A data frame with two columns: n-gram (bigram or trigram); count.
#' @export
#'
#' @examples library(janeaustenr)
#' d <- tibble(txt = janeaustenr::prideprejudice)
#' text_get_top_ngrams(d, "txt", ngram_type = 'bigram', min_freq = 10)

text_get_top_ngrams <- function(df, text_col_name, ngram_type = c('bigram', 'trigram'), min_freq = 1){

  ngrams_n <- ifelse(ngram_type == 'bigram', 2,
                     ifelse(ngram_type == 'trigram', 3,
                            stop("ngram_type must be 'bigram' or 'trigram'")))

  ngrams <- df %>%
    tidytext::unnest_tokens(output = ngram,
                            input = !! rlang::sym(text_col_name),
                            token = "ngrams",
                            n = ngrams_n) %>%
    tidyr::separate(ngram,
                    paste0("word", 1:ngrams_n),
                    sep = " ") %>%
    dplyr::filter( # filter out stop words
      dplyr::across(
        dplyr::starts_with("word"),
        ~ !. %in% tidytext::stop_words$word
      )
    ) %>%
    tidyr::unite(
      col = "ngram",
      paste0("word", 1:ngrams_n),
      sep = " "
    ) %>%
    dplyr::count(ngram, sort = TRUE) %>%
    dplyr::filter(n>= min_freq)

  return(ngrams)

}
