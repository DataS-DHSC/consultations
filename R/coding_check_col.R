#' Check if words or regex appears in column
#'
#' This helper function lets you efficiently search for a list of words within a question, and re-code those responses.
#' For example, you could use this inside a dplyr::case_when() call to re-code any responses that mention the word 'test' into a 'Test' response group.
#'
#' @param list_words vector of words or regular expressions
#' @param column dataframe column (as vector)
#'
#' @return boolean vector (true / false)
#' @export
#'
#' @examples coding_check_col(c('test', 'words'), c('this is test response 1', 'this is response 2', 'these are some words'))
coding_check_col <- function(list_words, column){
  grepl(paste0('(\\s?', paste0(list_words, collapse = '\\s?)|(\\s?'), '\\s?)'), tolower(column), perl = T)
}
