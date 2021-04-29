#' Extract and clean responses
#'
#' Take a consultation response spreadsheet and turn it into a list of neat tibbles of values and frequencies.
#' Removes non-responses, so calculates percentages of answers as a proportion of individuals who actually answered that question.
#'
#' Aggregates small groups of answers into an "Other (Aggregated)" group.
#'
#' @param dummy_response dataframe
#' @param qtypes list with elements categorical, multichoice, and freetext containing vectors of column names
#' @param min_n smallest group of answers allowed (statistical disclosure limit)
#'
#' @return list of tibbles
#' @export
#'
#' @examples survey_response_tables(dummy_response, survey_question_types(dummy_response))
survey_response_tables <- function(dummy_response, qtypes, min_n = 10){
  response_t <- list()
  for (i in colnames(dummy_response)){
    column <- dummy_response[, i]

    # Save factor levels
    if(is.factor(column)){
      col_levels <- levels(column)
    }

    # Remove non-responses
    column <- column[column != "" & !is.na(column)]

    # Prepare table based on column type
    if(length(column) > 0){
      if(i %in% qtypes$categorical){
        # For categorical, simple frequency table
        data_prep <- as.data.frame(table(column), stringsAsFactors = FALSE)
      } else if (i %in% qtypes$multichoice) {
        # For multi-choice first split answers by comma
        data_prep <- table(unlist(strsplit(column, ",(?!\\s)", perl = TRUE))) %>%
          as.data.frame(., stringsAsFactors = FALSE) %>%
          dplyr::rename(column = Var1)
      } else {
        data_prep <- data.frame(column = "Free text", Freq = length(column), stringsAsFactors = FALSE)
      }

      # Clean data table
      response_t[[i]] <- data_prep %>%
        # Remove empty responses
        dplyr::filter(nchar(column) > 0) %>%
        # Aggregate small groups to protect against statistical disclosure
        dplyr::mutate(Response = dplyr::case_when(Freq < min_n ~ "Other (Aggregated)",
                                                  Freq >= min_n ~ column)) %>%
        dplyr::group_by(Response) %>%
        dplyr::summarise(Freq = sum(Freq)) %>%
        # With multi-choice, percentages can add up to more than 100 because individuals can answer multiple times
        dplyr::mutate(Percentage = round(Freq / length(column) * 100, 2)) %>%
        dplyr::select(Response, Frequency = Freq, Percentage)
      } else {
        response_t[[i]] <- data.frame(Response = dummy_response[1, i], Freq = nrow(dummy_response), Percentage = 100)
        }

    # Re-apply factor levels and sort
    if(is.factor(column)){
      response_t[[i]]$Response <- factor(response_t[[i]]$Response, levels = col_levels)
      response_t[[i]] <- dplyr::arrange(response_t[[i]], Response)
      }
    }


  return(response_t)
  }
