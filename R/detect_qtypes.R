#' Detect question types
#'
#' Looks at the possible answers in a column, and determines which type of question
#' is most likely. This is based on rules of thumb and can be adapted for different
#' uses.
#'
#' @param response_col column from response dataframe
#'
#' @return
#' @export
#'
#' @examples
detect_qtypes <- function(response_col,
                          unique_vals = 12,
                          split_perc = 0.5,
                          prop_total = 0.85,
                          prop_common = 0.05){
  responses <- as.data.frame(table(response_col))
  split_cats <- unlist(strsplit(response_col, ","))
  split_responses <- as.data.frame(table(split_cats))
  # If fewer than x unique values, we treat it as categorical
  if(nrow(responses) < unique_vals){
    return("categorical")
    # If splitting the answers by comma, results in less than x%
    # as many categories, we consider it multi-choice
  } else if (nrow(split_responses) < split_perc*nrow(responses)) {
    return("multi-choice")
    # If the 5 most common values account for over x% of responses,
    # we treat it as categorical
  } else if (sum(dplyr::top_n(responses, 5)$Freq) >
           prop_total*sum(responses$Freq)){
    # If the most common response at most x% of responses,
    # we treat it as free-text
    return("categorical")
  } else if (max(responses$Freq) < prop_common*sum(responses$Freq)) {
    return("free text")
  } else {
    return("free text")
  }
}
