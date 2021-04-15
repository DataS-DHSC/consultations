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
question_types(dummy_response)

## ----response tables----------------------------------------------------------
response_tables(dummy_response, question_types(dummy_response), min_n = 5)

## ----barplots-----------------------------------------------------------------
survey_response_barplot(dummy_response)

## ----summary text-------------------------------------------------------------
lapply(response_tables(dummy_response, qtypes = question_types(dummy_response)), summary_text)

