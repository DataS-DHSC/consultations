#' Create regex pattern from list of pre-set answer options
#'
#' @param manifest vector of pre-set answer options
#'
#' @return regular expression string
#' @export
#'
#' @examples coding_create_pattern(c('answer option 1', 'answer option 2'))
coding_create_pattern <- function(manifest){
  # Account for punctuation with regex meaning
  man_improve <- gsub("\\(", "\\\\\\(", manifest)
  man_improve <- gsub("\\)", "\\\\\\)", manifest)
  paste0("(", paste0(man_improve, collapse = ")|("), ")")
}
