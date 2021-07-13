#' Combine pre-set and coded multi-code responses
#'
#' After extracting the original options (coding_create_orig) and grouping (coding_check_col) the other (coding_create_other) responses,
#' this function combines them together.
#'
#' @param orig string vector as produced by coding_create_orig()
#' @param other string vector as produced by coding_create_other()
#'
#' @return string vector
#' @export
#'
#' @examples coding_create_total(c('option 1,option 2'), c('typed option'))
coding_create_total <- function(orig, other){
  total <- paste(orig, other, sep = ",")
  # Remove 'NA' strings
  total <- gsub("NA", "", total)
  # Remove repeated commas
  total <- gsub(",+", ",", total)
  # Remove leading or lagging commas
  total <- gsub("(^,)|(,$)", "", total)
  return(total)
}
