#' Select best number of topics for LDA
#'
#' @param data_dtm Document-Term matrix as prepared by \code{\link{text_dtm_prep}}
#' @param k_opts integer vector of numbers of topics to be considered
#'
#' @return single integer: best number of topics
#' @export
#'
#' @examples text_dtm_prep(dummy_response, colnames(dummy_response)[7], 'response_id') %>%
#' text_best_n_topics(., seq(5, 10))
text_best_n_topics <- function(data_dtm, k_opts){
  # Apply LDA repeatedly, once for each possible k
  lda_loop <- lapply(k_opts, function(x) text_lda_dtm(data_dtm, k = x))

  # Select the log Likelihood from each model, find the maximum value, and return the k-value for this
  best_k <- k_opts[which.max(unlist(lapply(lda_loop, `[`, 'logLik')))]
  return(list(lda_loop = lda_loop,
              best_k = best_k))
}
