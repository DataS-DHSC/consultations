#' Extract only pre-set options from a multi-code question
#'
#' Where a column contains a "tick all that apply" type multi-code question, you want to treat the pre-set options just as they are.
#' This function allows you to extract just those responses, if you have a manifest pattern.
#' A manifest pattern is a vector of the pre-set options, prepared to have a regular expression shape.
#' You can do this using the coding_create_pattern function from this package.
#'
#' @param column dataframe column (as vector)
#' @param manifest_pattern string with regular expression pattern
#'
#' @return string vector with comma-separated pre-set options
#' @export
#'
#' @examples coding_create_orig(c('pre-set,manual', 'manual'), '(pre-set)')
coding_create_orig <- function(column, manifest_pattern){
  orig <-  stringr::str_extract_all(column, pattern = manifest_pattern)
  orig <- unlist(lapply(orig, paste, collapse = ","))
  return(orig)
}
