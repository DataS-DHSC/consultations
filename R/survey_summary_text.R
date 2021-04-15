#' Summarise categorical responses as text
#'
#' Take a question frequency table (as produced by response_tables), and summarise the responses as three-bullet text.
#' Requires the frequency table to have 'Response', 'Frequency', and 'Percentage' columns.
#' To apply to the full output of response_tables, use lapply.
#'
#' @param survey_response list
#'
#' @return glue string
#' @export
#'
#' @examples lapply(survey_response_tables(dummy_response, qtypes = survey_question_types(dummy_response)), survey_summary_text)
survey_summary_text <- function(survey_response){
  # Get the largest and smallest category
  max_cat <- dplyr::slice(survey_response, which.max(Frequency))
  min_cat <- dplyr::slice(survey_response, which.min(Frequency))

  # Generate the bullet point text
  text <- glue::glue('* There were {dplyr::n_distinct(survey_response$Response)} response categories ({paste(unique(survey_response$Response), collapse=", ")}).
  * The category with the highest number of responses was "{max_cat$Response}" with {max_cat$Frequency} responses ({max_cat$Percentage}%).
  * The category with the lowest number of responses was "{min_cat$Response}" with {min_cat$Frequency} responses ({min_cat$Percentage}%).')
  return(text)
}
