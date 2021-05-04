#' Remove responses with keywords
#'
#' This function may be useful if you want to remove responses which mention a particular word or phrase.
#' Note: this removes the whole response, not only the word itself - if you want to
#' remove one word only, use the text_remove_words() function.
#'
#' @param data data.frame containing all responses
#' @param text_col column containing free text
#' @param keywords responses containing any of these words will be removed. Not case-sensitive.
#'
#' @return data.frame with responses that match keywords removed
#' @export
#'
#' @examples text_drop_keyword_responses(dummy_response, colnames(dummy_response)[7], 'malesuada')
text_drop_keyword_responses <- function(data, text_col, keywords = c("insert_keywords")){
  # Turn keywords into regex pattern
  regex_pattern <- paste0("\\b(", paste0(tolower(keywords), collapse = "|"), ")\\b")
  # set text to lowercase
  match_text <- tolower(data[[text_col]])

  # Filter only rows where keywords do not appear
  data_filtered <- data[!grepl(regex_pattern, match_text),]

  return(data_filtered)
}
