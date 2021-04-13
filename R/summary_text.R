#' Summarise categorical responses as text
#'
#' Take a consultation response spreadsheet (as transformed by Hmisc::describe), and summarise the responses as three-bullet text.
#'
#' @param survey_response list
#'
#' @return string
#' @export
#'
#' @examples summary_text(Hmisc::describe(dummy_response))
summary_text <- function(survey_response){
  survey_response %>%
    glue::glue_data('* There were {dplyr::n_distinct(Response)} response categories ({paste(unique(Response), collapse=", ")}).
                    * The category with the highest number of responses was "{slice(., which.max(Frequency)) %>% pull(Response)}" with {slice(., which.max(Frequency)) %>% pull(Frequency)} responses ({slice(.which.max(Frequency %>% pull(Percentage)}%).
                    * The category with the lowest number of responses was "{slice(., which.min(Frequency)) %>% pull(Response)}" with {slice(., which.min(Frequency)) %>% pull(Frequency)} responses ({slice(which.min(Frequency %>% pull(Percentage)}%).')
}
