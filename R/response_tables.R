#' Extract and clean responses
#'
#' Take a consultation response spreadsheet and turn it into a list of neat tibbles of values and frequencies.
#'
#' @param dummy_response dataframe
#' @param qtypes list with elements categorical, multichoice, and freetext containing vectors of column names
#' @param min_n smallest group of answers allowed (statistical disclosure limit)
#'
#' @return list of tibbles
#' @export
#'
#' @examples response_tables(dummy_response, question_types(dummy_response))
response_tables <- function(dummy_response, qtypes, min_n = 5){
  response_t <- list()
  for (i in colnames(dummy_response)){
    if(i %in% qtypes$categorical){
      # For categorical, simple frequency table
      data_prep <- as.data.frame(table(dummy_response[, i]), stringsAsFactors = FALSE)
      } else if (i %in% qtypes$multichoice) {
        # For multi-choice first split answers by comma
        data_prep <- table(unlist(strsplit(dummy_response[, i], ","))) %>%
        as.data.frame(., stringsAsFactors = FALSE)
      } else {
        data_prep <- data.frame(Var1 = "Free text", Freq = 9999, Percentage = 9999, stringsAsFactors = FALSE)
      }
    # Clean data table
    response_t[[i]] <- data_prep %>%
      # Aggregate small groups to protect against statistical disclosure
      dplyr::mutate(Response = dplyr::case_when(Freq < min_n ~ "Other (Aggregated)",
                                                Freq >= min_n ~ Var1)) %>%
      dplyr::group_by(Response) %>%
      dplyr::summarise(Freq = sum(Freq)) %>%
      # With multi-choice, percentages can add up to more than 100 because individuals can answer multiple times
      dplyr::mutate(Percentage = round(Freq / nrow(dummy_response) * 100, 2)) %>%
      dplyr::select(Response, Frequency = Freq, Percentage)
  }
  return(response_t)
}
