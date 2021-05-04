#' Suppress small groups in data
#'
#' Replaces values that appear fewer than min_n times with "Other (Aggregated)".
#' This ensures that in later summaries, these answers are grouped together.
#' This serves to help prevent statistical disclosure.
#' However, continue to check your outputs for small cells and aggregate up where necessary.
#'
#' @param column vector of values to modify
#' @param min_n minimum group size to keep
#'
#' @return vector of values with rare values replaced
#' @export
#'
#' @examples survey_suppress_small_numbers(c(rep("test", 15), rep("test2", 5), NA), min_n = 10)
survey_suppress_small_numbers <- function(column, min_n = 10){
  # Find values that appear fewer than n times
  rare_values <- names(which(table(column) < min_n))
  # Replace those values with "Other (Aggregated)"
  column[column[[1]] %in% rare_values, ] <- "Other (Aggregated)"

  return(column)
}
