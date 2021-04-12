#' Extract and clean responses
#'
#' Take a consultation response spreadsheet (as transformed by Hmisc::describe), and turn it into a neat tibble of values and frequencies.
#'
#' @param survey_response list
#'
#' @return tibble
#' @export
#'
#' @examples response_tables(survey_response)
response_tables <- purrr::safely(function(survey_response){
  survey_response$values$frequency %>%
    tibble::as_tibble() %>%
    dplyr::rename(Frequency = value) %>%
    dplyr::mutate(Response = survey_response$values$value,
                  Response = dplyr::case_when(Frequency < 5 ~ "Other (Aggregated)",
                                              Frequency >= 5 ~ Response,
                                              TRUE ~ "Missing frequency")) %>%
    dplyr::group_by(Response) %>%
    dplyr::summarise(Frequency = sum(Frequency)) %>%
    dplyr::mutate(Percentage = round((Frequency/sum(Frequency))*100, 2)) %>%
    dplyr::select(Response, Frequency, Percentage)
})
