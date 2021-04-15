## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(consultations)
data(dummy_response, package = "consultations")

## ----dummy data---------------------------------------------------------------
knitr::kable(dummy_response[1:5,])

## ----qtypes-------------------------------------------------------------------
survey_question_types(dummy_response)

## ----response tables----------------------------------------------------------
survey_response_tables(dummy_response, survey_question_types(dummy_response), min_n = 5)

## ----barplots-----------------------------------------------------------------
survey_response_barplot(dummy_response)

## ----summary text-------------------------------------------------------------
lapply(survey_response_tables(dummy_response, qtypes = survey_question_types(dummy_response)), survey_summary_text)

