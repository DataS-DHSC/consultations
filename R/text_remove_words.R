#' Remove words before analysis
#'
#' After unnesting the words from the full free-text column, you may want to filter out certain groups of words:
#' stopwords like 'and' or 'the', profanity, number-only words, or a list of custom words.
#' This function lets you do this easily.
#'
#' @param unnest_data dataframe with unnested free text data (a row per word, eg. as prepared by tidytext::unnest_tokens)
#' @param word_col column name containing word tokens
#' @param stopwords do you want to remove stopwords? TRUE/FALSE
#' @param profanity do you want to remove profanity? TRUE/FALSE
#' Default profanity list here: https://www.cs.cmu.edu/~biglou/resources/bad-words.txt
#' @param number_only do you want to remove "words" that are only numbers?
#' These are usually years or phone numbers. TRUE/FALSE
#' @param custom_words do you have a custom list of words to remove? Must be entered as a string vector.
#'
#' @return dataframe with rows filtered out
#' @export
#'
#' @examples text_remove_words(data.frame(doc_id = c(1, 2, 3, 4), word = c('1', 'test', 'the', 'function')), word_col = 'word')
text_remove_words <- function(unnest_data, word_col = "word", stopwords = TRUE, profanity = TRUE, number_only = TRUE, custom_words = c("")){
  data_out <- unnest_data
  # Remove stopwords?
  if(stopwords){
    data_out <- dplyr::anti_join(data_out, tidytext::get_stopwords(), by = word_col)
  }
  # Remove profanity?
  if(profanity){
    prof <- read.table('https://www.cs.cmu.edu/~biglou/resources/bad-words.txt', stringsAsFactors = FALSE)
    colnames(prof) <- word_col
    data_out <- dplyr::anti_join(data_out, prof, by = word_col)
  }
  # Remove number-only words?
  if(number_only){
    data_out <- data_out[!(grepl('^[0-9]{1,}$', data_out[[word_col]])),]
  }
  # Remove custom word list?
  if(length(custom_words) > 0){
    data_out <- data_out[!(data_out[[word_col]] %in% custom_words),]
  }

  return(data_out)
}
