% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_prep_multiword_phrases.R
\name{text_prep_multiword_phrases}
\alias{text_prep_multiword_phrases}
\title{Prepare multi-word phrases before unnesting}
\usage{
text_prep_multiword_phrases(text_col, multiword_list = c(""))
}
\arguments{
\item{text_col}{string vector (or dataframe column)}

\item{multiword_list}{vector of multi-word phrases you want to treat as}
}
\value{
string vector with multi-word phrases replaced in PascalCase
}
\description{
This function removes spaces from multi-word phrases, so they will be treated as a single word.
This can be helpful when there are domain-specific multi-word phrases that you want to analyse as one term.
Apply this function to your free-text columns before unnesting words.
}
\details{
Returns multi-word phrases as a single word in PascalCase.
}
\examples{
text_prep_multiword_phrases(c("test this", "not a test"), multiword_list = c("test this"))
}
