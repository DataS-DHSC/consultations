#' Prepare multi-word phrases before unnesting
#'
#' This function removes spaces from multi-word phrases, so they will be treated as a single word.
#' This can be helpful when there are domain-specific multi-word phrases that you want to analyse as one term.
#' Apply this function to your free-text columns before unnesting words.
#'
#' Returns multi-word phrases as a single word in PascalCase.
#'
#' @param text_col string vector (or dataframe column)
#' @param multiword_list vector of multi-word phrases you want to treat as
#'
#' @return string vector with multi-word phrases replaced in PascalCase
#' @export
#'
#' @examples text_prep_multiword_phrases(c("test this", "not a test"), multiword_list = c("test this"))
text_prep_multiword_phrases <- function(text_col, multiword_list = c("")){
  replace_phrases <- gsub("\\s", "", stringr::str_to_title(multiword_list))
  names(replace_phrases) <- paste0("(", multiword_list, ")")

  replaced <- stringr::str_replace_all(text_col,
                           stringr::regex(replace_phrases,
                                          ignore_case = TRUE))
  return(replaced)
}
