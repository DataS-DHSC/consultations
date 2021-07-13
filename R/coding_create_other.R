#' Extract only manually entered text from a multi-code question
#'
#' For multi-code "tick all that apply" type questions, there is sometimes an "Other, please specify" option.
#' This function extracts only this section, for grouping into categories.
#'
#' @param column dataframe column (as vector)
#' @param manifest_pattern string with regular expression pattern
#'
#' @return string vector with comma-separated pre-set options
#' @export
#'
#' @examples coding_create_other(c('pre-set,manual', 'manual'), '(pre-set)')
coding_create_other <- function(column, manifest_pattern){
  other <- stringr::str_remove_all(column, pattern = manifest_pattern)
  # Remove left-over commas at the start and end
  other <- stringr::str_remove_all(other, pattern = "^(,+)|(,+$)")

  return(other)
}
