#' Prepare data for topic modelling
#'
#' Calculate TF-IDF scores and create a Document-Term Matrix from your dataset.
#'
#' @param unnest_data dataframe with unnested tokens
#' @param grouping_var column used for determining what consistutes a document; with quotation marks (string)
#' If each response is a row, add a column with row identifiers to be the grouping_var.
#' @param word_col column name containing the unnested tokens
#'
#' @return List containing the prepared data and a Document-Term Matrix
#' @export
#'
#' @examples tidytext::unnest_tokens(dummy_response, 'word', colnames(dummy_response)[7]) %>%
#' text_dtm_prep(., 'response_id')
#'
#'
text_dtm_prep <- function(unnest_data, grouping_var, word_col = "word"){
  grouping_var <- prep_grouping_var(grouping_var)
  unnest_data$word <- as.character(unnest_data[, word_col])

  # Count words per group
  prep_data <- unnest_data %>%
    dplyr::count(!!grouping_var, word, sort = TRUE) %>%
    dplyr::group_by(!!grouping_var) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::arrange(-n) %>%
    dplyr::mutate(rank = dplyr::row_number(),
                  `term frequency` = n/total) %>%
    tidytext::bind_tf_idf(word, !!grouping_var, n) %>%
    dplyr::arrange(desc(tf_idf)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(word = factor(word, levels = rev(unique(word))))

  dtm_prep <- tidytext::cast_dtm(prep_data, document = !!grouping_var, term = word, value = n)


  return(list(data_scored = prep_data,
              dtm_prep = dtm_prep))
}
