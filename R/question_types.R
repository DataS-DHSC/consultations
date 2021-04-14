#' Group columns into question types
#'
#' Returns a list containing vectors with column names for categorical, multi-choice, and free text questions.
#'
#' @param dummy_response dataframe
#'
#' @return three-element list
#' @export
#'
#' @examples question_types(dummy_response)
question_types <- function(dummy_response){
  # Apply question detection function
  question_types <- as.data.frame(apply(dummy_response, 2, detect_qtypes))
  question_types$question <- rownames(question_types)
  colnames(question_types) <- c("type", "question")

  # Return list with three vectors:
  # each vector containing the columns that have that type of question
  return(list(categorical = question_types[question_types$type == "categorical", "question"],
              multichoice = question_types[question_types$type == "multi-choice", "question"],
              freetext = question_types[question_types$type == "free text", "question"]))
}
