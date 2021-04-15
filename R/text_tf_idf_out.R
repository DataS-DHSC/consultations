#' Calculate TF-IDF scores
#'
#' @param data pre-cleaned response data
#' @param grouping_var column that groups each row. May be with (string) or without (tidy evaluation) quotation marks
#'
#' @return dataframe with a row for each group-word combination, and the tf-idf score
#' @export
#'
#' @examples text_tf_idf_out(text_unnest_stem_and_remove_stopwords(dummy_response, colnames(dummy_response)[7], "words"), colnames(dummy_response)[1])
text_tf_idf_out <- function(data, grouping_var){
  # Allow for both string input and tidyeval using quosures for the grouping_var
  if(is.character(grouping_var)){
    grouping_var <- sym(grouping_var)
  } else {
    grouping_var <- rlang::enquo(grouping_var)
  }


  tf_idf_out <- data %>%
    dplyr::count(!!grouping_var, word, sort = TRUE) %>%
    dplyr::group_by(!!grouping_var) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::arrange(-n) %>%
    dplyr::mutate(rank = row_number(),
           `term frequency` = n/total) %>%
    tidytext::bind_tf_idf(word, !!grouping_var, n) %>%
    dplyr::arrange(desc(tf_idf)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(word = factor(word, levels = rev(unique(word))))
  return(tf_idf_out)
}
