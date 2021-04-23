# Create dummy data
# This script serves only to generate the dummy_response data in this package.
# The data is entirely random, but has columns that are similar to the types of columns we would see in consultations.
# The dummy answers should not be seen as exhaustive lists of responses available in real consultations.


# Set size of dummy_response dataset
rows <- 100

# Lists of response options
respond <- c("an individual", "an organisation", "")
location <- c("England", "Northern Ireland", "Scotland",
              "Wales", "Outside the UK", "")
age <- c("16-17 years old", "18-19 years old", "20-24 years old", "25-29 years old",
         "30-39 years old", "40-49 years old", "50-59 years old", "60-69 years old",
         "70-79 years old", "Above 80 years old", "")
ethnicity <- c("White", "Mixed/Multiple ethnicities", "Asian/Asian British",
               "Black/African/Caribbean/Black British", "Other ethnic group", "")
contact <- c("Yes", "No", "")

# For multi-select categorical questions, we will randomly combine from a topic list
theme_opts <- c("TopicA", "Topic B", "TopicC ", "Topic D ", " Topic E")
selection_length <- round(runif(rows, min = 0, max = length(theme_opts)))
themes <- sapply(selection_length, FUN = function(x) paste(sample(theme_opts, size = x, replace = FALSE), collapse = ","))

# For free text, we will reshuffle the words from lorem ipsum
# This results in unusual capitalisation and punctuation, too.
words <- unlist(strsplit(paste0(stringi::stri_rand_lipsum(6, start_lipsum= FALSE), collapse = " "), split = " "))
text_length <- round(runif(rows, min = 0, max = 150), 0)
free_text <- sapply(text_length, FUN = function(x) paste(sample(words, size = x, replace = TRUE), collapse = " "))



# Randomly sample from each list to populate the columns
dummy_response <- data.frame(respond = sample(respond, size = rows, replace = TRUE),
                         location = sample(location, size = rows, replace = TRUE),
                         age = sample(age, size = rows, replace = TRUE),
                         ethnicity = sample(ethnicity, size = rows, replace = TRUE),
                         contact = sample(contact, size = rows, replace = TRUE),
                         themes = themes,
                         free_text = free_text,
                         response_id = seq(1, rows))
# Set interpretable column names
colnames(dummy_response) <- c("Are you completing this consultation as:",
                          "Where are you based?",
                          "What is your age?",
                          "What is your ethnicity?",
                          "May we contact you via email about your response?",
                          "Which themes would you like to share your responses about?",
                          "Please share your views on these themes:",
                          "response_id")
