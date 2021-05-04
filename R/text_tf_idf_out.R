#' Calculate TF-IDF scores
#'
#' @param data pre-cleaned response data
#' @param grouping_var column that groups each row. May be with (string) or without (tidy evaluation) quotation marks
#'
#' @return dataframe with a row for each group-word combination, and the tf-idf score
#' @export
#'
#' @examples tidytext::unnest_tokens(dummy_response, 'word', colnames(dummy_response)[7]) %>%
#' text_tf_idf_out(., colnames(dummy_response)[1])
#'
#'
text_tf_idf_out <- function(unnest_data, grouping_var){
  grouping_var <- prep_grouping_var(grouping_var)

  tf_idf_out <- unnest_data %>%
    dplyr::count(!!grouping_var, word, sort = TRUE) %>%
    dplyr::group_by(!!grouping_var) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::arrange(-n) %>%
    dplyr::mutate(rank = dplyr::row_number(),
           `term frequency` = n/total) %>%
    tidytext::bind_tf_idf(word, !!grouping_var, n) %>%
    dplyr::arrange(desc(tf_idf)) %>%
    dplyr::ungroup()
  return(tf_idf_out)
}
