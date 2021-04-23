#' Prepare data for topic modelling
#'
#' Create a Document-Term Matrix from your dataset.
#' Includes text preparation (unnesting, stopword removal, and stemming).
#' Also includes creation of tf-idf transformation.
#'
#' @param data dataframe containing responses
#' @param text_col column name containing free text
#' @param grouping_var column used for determining what consistutes a document; with quotation marks (string).
#' If each response is a row, add a column with row identifiers to be the grouping_var.
#'
#' @return Document-Term Matrix from responses dataframe
#' @export
#'
#' @examples text_dtm_prep(dummy_response, colnames(dummy_response)[7], 'response_id')


text_dtm_prep <- function(data, text_col, grouping_var){
  unnest_data <- text_unnest_remove_stem_words(data, text_col)
  prep_data <- text_tf_idf_out(unnest_data, grouping_var)


  grouping_var <- prep_grouping_var(grouping_var)
  dtm_inc <- prep_data %>%
    tidytext::cast_dtm(document = !!grouping_var, term = word, value = n)
  return(dtm_inc)
}
