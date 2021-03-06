% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_remove_words.R
\name{text_remove_words}
\alias{text_remove_words}
\title{Remove words before analysis}
\usage{
text_remove_words(
  unnest_data,
  word_col = "word",
  stopwords = TRUE,
  profanity = TRUE,
  number_only = TRUE,
  custom_words = c("")
)
}
\arguments{
\item{unnest_data}{dataframe with unnested free text data (a row per word, eg. as prepared by tidytext::unnest_tokens)}

\item{word_col}{column name containing word tokens}

\item{stopwords}{do you want to remove stopwords? TRUE/FALSE}

\item{profanity}{do you want to remove profanity? TRUE/FALSE
Default profanity list here: https://www.cs.cmu.edu/~biglou/resources/bad-words.txt}

\item{number_only}{do you want to remove "words" that are only numbers?
These are usually years or phone numbers. TRUE/FALSE}

\item{custom_words}{do you have a custom list of words to remove? Must be entered as a string vector.}
}
\value{
dataframe with rows filtered out
}
\description{
After unnesting the words from the full free-text column, you may want to filter out certain groups of words:
stopwords like 'and' or 'the', profanity, number-only words, or a list of custom words.
This function lets you do this easily.
}
\examples{
text_remove_words(data.frame(doc_id = c(1, 2, 3, 4), word = c('1', 'test', 'the', 'function')), word_col = 'word')
}
