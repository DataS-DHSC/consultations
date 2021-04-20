#' Pre-clean consultation responses
#'
#' Takes a dataframe with a free-text column as input. Unnests words from the free text, removes stopwords, profanity, and number-only words,
#' removes an optional custom list of words, and stems words (using hunspell) so they can be grouped better.
#'
#' Note that, although you may choose a token other than "words" (like "sentences"), the word removal and stemming expects word tokens only.
#'
#' @param data dataframe of responses
#' @param text_col name of column containing free text to be analysed
#' @param token what text entity to be analysed? by default word-by-word, but other options are defined in tidytext::unnest_tokens
#'
#' @return dataframe with stemmed, unnested words with removed words
#' @export
#'
#' @examples text_unnest_remove_stem_words(dummy_response, colnames(dummy_response)[7], "words")
text_unnest_remove_stem_words <- function(data, text_col, token = "words", custom_words = c("")){
  data %>%
    tidytext::unnest_tokens(output = word, input = text_col, token = token) %>%
    text_remove_words(., custom_words = custom_words) %>%
    text_stem_words(.)
}
