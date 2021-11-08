#' Calculate TF-IDFs for unigrams, bigrams or trigrams
#'
#' For two or more free-text columns, return the unigrams, bigrams or trigrams with the
#' largest TF-IDFs for the given columns.
#'
#' @param df A data frame with at least two free-text columns (i.e. questions).
#' @param q_cols A vector containing the names of the free-text columns.
#' @param ngrams_type A string. Should be "unigrams","bigrams" or "trigrams".
#' @param top_n_ngrams Integer. Number of top ngrams to return per question. Defaults to all.
#' @param with_ties Should ties be kept together? The default, FALSE, may return less rows than you request.
#'
#' @return A data frame with six columns: question name; n-gram (word, bigram or trigram); count;
#'     term-frequency; inverse document frequency; and TF-IDF.
#' @export
#'
#' @examples  df <- data.frame(q1 = c("hello", "good morning"), q2 = c("bye", "good night"))
#' text_calc_tfidf_ngrams(df, c("q1", "q2"), "unigrams")

text_calc_tfidf_ngrams <- function(df,
                              q_cols,
                              ngram_type = c("unigrams", "bigrams", "trigrams"),
                              top_n_ngrams = NULL,
                              with_ties = FALSE) {

  if(length(q_cols)==1){
    stop('q_cols needs to have at least two elements for tf-idf calculations')
  }

  # derive numeric type of ngram based on inputted string
  ngrams_n <- ifelse(ngram_type == 'unigrams', 1,
                     ifelse(ngram_type == 'bigrams', 2,
                            ifelse(ngram_type == 'trigrams', 3,
                                   stop("ngram_type must be 'unigrams', 'bigrams' or 'trigrams'"))))

  # pivot input dataframe, so there are two columns: question name and response text
  df_long <- tidyr::pivot_longer(df[,q_cols],
                            cols = q_cols,
                            names_to = "question_col",
                            values_to = "text_col")


  tfidf_ngrams <- df_long %>%
    tidytext::unnest_tokens(
      output = "ngram",
      input = "text_col",
      token = "ngrams",
      n = ngrams_n
    ) %>%
    tidyr::separate(
      ngram,
      paste0("word", 1:ngrams_n),
      sep = " "
    ) %>%
    dplyr::filter( # Do this because some stop words make it through the TF-IDF filtering that happens below.
      dplyr::across(
        dplyr::starts_with("word"),
        ~ !. %in% tidytext::stop_words$word
      )
    ) %>%
    tidyr::unite(
      col = "ngram", paste0("word", 1:ngrams_n),
      sep = " "
    ) %>%
    dplyr::count(
      question_col,
      ngram,
      sort = TRUE
    ) %>%
    tidytext::bind_tf_idf(
      ngram,
      question_col,
      n
    )

  # if top_n_ngrams is provided, only keep top n ngrams for each question
  if(!is.null(top_n_ngrams)){
    tfidf_ngrams <- tfidf_ngrams %>%
      dplyr::group_by(question_col) %>%
      dplyr::slice_max(tf_idf, n = top_n_ngrams, with_ties = with_ties) %>%
      dplyr::ungroup()
  }

  return(tfidf_ngrams)
}
