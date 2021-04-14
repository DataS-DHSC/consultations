#' Extract and clean responses
#'
#' Take a consultation response spreadsheet and turn it into a list of neat tibbles of values and frequencies.
#'
#' @param dummy_response dataframe
#'
#' @return list of tibbles
#' @export
#'
#' @examples response_tables(dummy_response, question_types(dummy_response))
response_tables <- function(dummy_response, qtypes, min_n = 5){
  response_t <- list()
  for (i in colnames(dummy_response)){
    if(i %in% qtypes$categorical){
      response_t[[i]] <- as.data.frame(table(dummy_response[, i]), stringsAsFactors = FALSE) %>%
          dplyr::mutate(Response = dplyr::case_when(Freq < min_n ~ "Other (Aggregated)",
                                                    Freq >= min_n ~ Var1)) %>%
          dplyr::mutate(Percentage = round((Freq/sum(Freq))*100, 2)) %>%
          dplyr::select(Response, Frequency = Freq, Percentage)
    } else if (i %in% qtypes$multichoice) {
      # For multi-choice first split by comma
      response_t[[i]] <- table(unlist(strsplit(dummy_response[, i], ","))) %>%
        as.data.frame(., stringsAsFactors = FALSE) %>%
        dplyr::mutate(Response = dplyr::case_when(Freq < min_n ~ "Other (Aggregated)",
                                                  Freq >= min_n ~ Var1)) %>%
        # With multi-choice, percentages can add up to more than 100 because individuals can answer multiple times
        dplyr::mutate(Percentage = round(Freq / nrow(dummy_response) * 100, 2)) %>%
        dplyr::select(Response = Var1, Frequency = Freq, Percentage)
      } else {
    response_t[[i]] <- data.frame(Response = "Free text", Frequency = 0, Percentage = 0, stringsAsFactors = FALSE)
      }
  }
  return(response_t)
}
