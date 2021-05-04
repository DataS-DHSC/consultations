#' Set grouping variable to tidyeval
#'
#' @param grouping_var with quotation marks (string) or without (tidyeval)
#'
#' @return value prepared for tidy evaluation
#' @export
#'
#' @examples prep_grouping_var('column_1')


prep_grouping_var <- function(grouping_var){
  # Allow for both string input and tidyeval using quosures for the grouping_var
  if(is.character(grouping_var)){
    grouping_var <- as.name(grouping_var)
  }
  grouping_var <- rlang::enquo(grouping_var)
  return(grouping_var)
}
