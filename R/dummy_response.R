#' Dummy data with some relevant columns
#'
#' This data was generated in this package.
#' The data is entirely random, but has columns that are similar to the types of columns we would see in consultations.
#' The dummy answers should not be seen as exhaustive lists of responses available in real consultations.
#'
#' @format A data frame with 100 rows and 8 variables:
#' \describe{
#'   \item{Are you completing this consultation as:}{an individual or an organisation}
#'   \item{Where are you based?}{Countries of the UK, or outside the UK}
#'   \item{What is your age?}{In age brackets}
#'   \item{What is your ethnicity?}{Based on ONS default ethnic groups}
#'   \item{May we contact you via email about your response?}{Yes or no}
#'   \item{Which themes would you like to share your responses about?}{Topics A to E, concatenated}
#'   \item{Please share your views on these themes}{Randomly generated lorem ipsum free text}
#'   \item{response_id}{Sequence of row IDs, signifying each row is a single response}
#' }
#' @source generated in data-raw
"dummy_response"
