#' Detect question types
#'
#' Looks at the possible answers in a column, and determines which type of question
#' is most likely. This is based on rules of thumb and can be tuned for different
#' uses using the calculation criteria.
#'
#' Calculation criteria can be set by the user:
#' `unique_vals`: what's the maximum number of unique values a column can have, to always consider it categorical?
#' `split_perc`: determines which questions are deemed multi-choice: if the answers are split by comma, how much fewer unique values should that column have than before splitting? Helps distinguish between free-text and multi-choice.
#' `prop_total`: if the 5 most common values account for this proportion of the total number of answers, consider it categorical
#' `prop_common`: if the most common response is at most this proportion of the total, consider it free text
#'
#' @param response_col column from response dataframe
#' @param unique_vals integer
#' @param split_perc float
#' @param prop_total float
#' @param prop_common float
#'
#' @return a string value
#' @export
#'
#' @examples apply(dummy_response, 2, survey_detect_qtypes)
survey_detect_qtypes <- function(response_col,
                          unique_vals = 20,
                          split_perc = 0.5,
                          prop_total = 0.55,
                          prop_common = 0.05){
  # Remove non-responses
  response_col <- response_col[nchar(response_col) > 0]
  # Prepare responses data frequency table
  responses <- as.data.frame(table(response_col), stringsAsFactors = FALSE) %>%
    # Sort responses by frequency
    .[order(.$Freq, decreasing = TRUE),]

  # Split responses by commas (only if no whitespace afterwards)
  if(any(grep(",", response_col))){
    split_responses <- as.data.frame(table(unlist(strsplit(response_col, ",(?!\\s)", perl = TRUE))), stringsAsFactors = FALSE) %>%
      # Sort responses by frequency
      .[order(.$Freq, decreasing = TRUE),]
  } else {
    split_responses <- as.data.frame(table(response_col), stringsAsFactors = FALSE) %>%
      # Sort responses by frequency
      .[order(.$Freq, decreasing = TRUE),]
    }


  if (length(responses) > 0) {
      # If fewer than x unique values, we treat it as categorical
    if (nrow(responses) < unique_vals){
      return("categorical")

      # If splitting the answers by comma results in less than x%
      # as many categories, we consider it multi-choice
    } else if (nrow(split_responses) < split_perc*nrow(responses)) {
      return("multi-choice")

      # If splitting the answers by comme results in less than x
      # unique values, we treat it as multi-choice
    } else if(nrow(responses) < unique_vals){
      return("multi-choice")

      # If the 5 most common values account for over x% of responses,
      # we treat it as categorical
    } else if (sum(responses$Freq[1:5]) > prop_total*sum(responses$Freq)){
      return("categorical")


      # If the 5 most common values after splitting account for over x% of responses,
      # we treat it as multi-choice
    } else if (sum(split_responses$Freq[1:5]) > prop_total*sum(responses$Freq)){
      return("multi-choice")

      # If the most common response at most x% of responses,
      # we treat it as free-text
    } else if (max(responses$Freq) < prop_common*sum(responses$Freq)) {
      return("free text")

      # For other types, we consider free text
    } else {
      return("free text")
    }
  } else {
    return("empty")
  }
}
