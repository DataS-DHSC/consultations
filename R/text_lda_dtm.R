#' Calculate LDA topic model
#'
#' @param data_dtm Document-Term matrix as prepared by text_dtm_prep
#' @param grouping_var column used for determining what consistutes a document; with quotation marks (string) or without (tidyeval)
#' @param k Number of topics to detect
#' @param burnin parameter for Gibbs LDA
#' @param iter parameter for Gibbs LDA
#' @param keep parameter for Gibbs LDA
#'
#' @return list with elements: lda_out (full LDA topicmodel), beta (beta scores), gamma (gamma scores), and logLik (Log likelihood of topicmodel object)
#' @export
#'
#' @examples text_dtm_prep(dummy_response, colnames(dummy_response)[7], colnames(dummy_response)[1]) %>%
#' text_lda_dtm(., colnames(dummy_response)[1])
text_lda_dtm <- function(data_dtm, grouping_var, k = 2, burnin = 1000, iter = 1000, keep = 50){
  lda_out <- topicmodels::LDA(data_dtm, k = k,
                              method = "Gibbs",
                              control = list(burnin = burnin,
                                             iter = iter,
                                             keep = keep,
                                             seed = 1234))

  betas   <- broom::tidy(lda_out, matrix = "beta")
  gammas  <- broom::tidy(lda_out, matrix = "gamma")

  logLik <- topicmodels::logLik(lda_out)

  return(list(lda_out = lda_out,
              beta = betas,
              gamma = gammas,
              logLik = logLik))
}
