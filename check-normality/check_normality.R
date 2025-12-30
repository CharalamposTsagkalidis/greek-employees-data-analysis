library(readxl)
library(dplyr)

# dataset
data <- read_excel("data/Questionnaire of Thesis MBA (Responses).xlsx")

# Vector with question
likert_questions <- c(
  "How important is it for you to know the purpose/final goal of the project you undertake?",
  "I feel that the work environment does not support my personal or professional development.",
  "How important is it for you to have someone who motivates you in your work?",
  "My leader/manager motivates me to perform at my best.",
  "How much does micromanagement reduce your motivation at work?",
  "The level of organization in a project significantly affects my motivation.",
  "How does working 37 hours a week influence your motivation?",
  "Do you feel fear of being laid off at your current job"
)

# Select only Likert columns
likert_df <- data %>% select(all_of(likert_questions))

# Shapiro–Wilk test for every Likert question
normality_results <- lapply(likert_df, shapiro.test)


for (i in seq_along(normality_results)) {
  cat("\n-------------------------------------------------\n")
  cat("Variable:", names(likert_df)[i], "\n")
  cat("Shapiro-Wilk p-value:", normality_results[[i]]$p.value, "\n")
  if(normality_results[[i]]$p.value > 0.05){
    cat("=> The data follow a normal distribution.\n")
  } else {
    cat("=> The data do not follow a normal distribution.\n")
  }
}
