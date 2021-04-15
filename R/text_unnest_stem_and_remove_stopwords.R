#' Pre-clean consultation responses
#'
#' @param data dataframe of responses
#' @param text_col name of column containing free text to be analysed
#' @param token what text entity to be analysed? by default word-by-word, but other options are defined in tidytext::unnest_tokens
#'
#' @return dataframe with stemmed, unnested words with removed stopwords
#' @export
#'
#' @examples text_unnest_stem_and_remove_stopwords(dummy_response, colnames(dummy_response)[7], "words")
text_unnest_stem_and_remove_stopwords <- function(data, text_col, token = "words"){
  dummy_response %>%
    tidytext::unnest_tokens(output = word, input = text_col, token = token) %>%
    dplyr::anti_join(tidytext::get_stopwords(), by = "word") %>% # Remove stop-words
    dplyr::filter(!grepl("^[0-9]{1,}$", word)) %>% # Get rid of number-only 'words'
    dplyr::filter(!grepl("^[vxi]+$", word)) %>% # Get rid of roman numerals
    dplyr::mutate(word = hunspell::hunspell_stem(word)) %>% # Stem words to group better
    tidyr::unnest(word)
}
