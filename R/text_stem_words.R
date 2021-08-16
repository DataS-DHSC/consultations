#' Stem words before analysis
#'
#' @param unnest_data dataframe with unnested free text data (a row per word, eg. as prepared by tidytext::unnest_tokens)
#' @param word_col column name containing word tokens
#' @param except optional character vector containing words that should not be stemmed
#'
#' @return dataframe with words replaced with their porter stemmed version
#' @export
#'
#' @examples text_stem_words(data.frame(doc_id = c(1, 2, 3, 4), word = c('test', 'testing', 'tester', 'word')), word_col = 'word')
text_stem_words <- function(unnest_data, word_col = "word", except = ""){
  # Stem words
  stem_words <- SnowballC::wordStem(unnest_data[[word_col]])

  # Replace original words with stemmed words unless an exception
  unnest_data[[word_col]] <- ifelse((is.na(stem_words) | unnest_data[[word_col]] %in% tolower(except)), unnest_data[[word_col]], stem_words)

  return(unnest_data)
}
