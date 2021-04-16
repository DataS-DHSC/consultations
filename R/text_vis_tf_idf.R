#' Visualise tf-idf output
#'
#' @param tfidf_data dataframe as produced by text_tf_idf_out function
#' @param grouping_var a column name, with quotes (string) or without (tidyeval)
#' @param top_n Top x number of words by tf-idf score
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples text_unnest_stem_and_remove_stopwords(dummy_response, colnames(dummy_response)[7], "words") %>%
#' text_tf_idf_out(., colnames(dummy_response)[1]) %>%
#' text_vis_tf_idf(., colnames(dummy_response)[1])
#'
text_vis_tf_idf <- function(tfidf_data, grouping_var, top_n = 10){
  grouping_var <- prep_grouping_var(grouping_var)

  tfidf_data %>%
    dplyr::group_by(!!grouping_var) %>%
    dplyr::top_n(top_n, tf_idf) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(word = reorder(word, tf_idf)) %>%
    ggplot2::ggplot(ggplot2::aes(word, tf_idf, fill = !!grouping_var)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::labs(title = paste0("TF-IDF by ", dplyr::as_label(grouping_var)),
                  x = NULL, y = "tf-idf") +
    ggplot2::facet_wrap(ggplot2::vars(!!grouping_var), ncol = 2, scales = "free") +
    ggplot2::coord_flip()
}
