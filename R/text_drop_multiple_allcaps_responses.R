#' Remove responses with multiple all-caps words
#'
#' In some cases, you don't want to include responses that are written fully capitalised.
#' However, you must be careful to ignore legitimate all-caps words, such as acronyms in the domain of the consultation (eg. 'NHS').
#'
#' @param data data.frame containing all responses
#' @param text_col column containing free text
#' @param max_cap maximum number of all-caps words to allow per response
#' @param ignore_caps string vector of all-caps words to ignore in this count (eg. legitimate acronyms)
#'
#' @return data.frame of responses with all-caps responses filtered out
#' @export
#'
#' @examples text_drop_multiple_allcaps_responses(data.frame(doc_id = c(1, 2), text = c('TEST THE FUNCTION', 'TEST NHS FUNCTION')), text_col = 'text', max_cap = 2)
text_drop_multiple_allcaps_responses <- function(data, text_col, max_cap = 3, ignore_caps = c("NHS")){
  # Remove all-caps words to ignore
  # Turn to-be-ignored words into regex pattern
  regex_pattern <- paste0("\\b(", paste0(ignore_caps, collapse = "|"), ")\\b")
  # Remove to-be-ignored words from original text
  data_removed <- gsub(regex_pattern, "", data[[text_col]])

  # Count number of all-caps words per response
  num_allcaps <- stringr::str_count(data_removed, "(^| )[A-Z]+?( |$)")

  # Remove responses with too many capitalised words
  data_dropped <- data[num_allcaps < max_cap,]

  return(data_dropped)
}
