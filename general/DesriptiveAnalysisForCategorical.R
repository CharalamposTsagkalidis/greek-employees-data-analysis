library(ggplot2)
library(psych)
library(readxl)
library(dplyr)
data <- read_excel("data/Questionnaire of Thesis MBA (Responses).xlsx")
categorical_data <- data %>%
  select(where(is.character))


names(categorical_data) <- c(
  "BiologicalSex",
  "AgeGroup",
  "Education",
  "WorkExperienceYears",
  "WorkMotivation",
  "LeadershipType",
  "LongTermPurpose",
  "ReactionToLowMotivation"
)


freq_counts <- lapply(categorical_data, table, useNA = "ifany")


freq_percent <- lapply(categorical_data, function(x) round(prop.table(table(x)) * 100, 1))

# Print results for each question
for (i in names(categorical_data)) {
  cat("\n---", i, "---\n")
  print(freq_counts[[i]])
  cat("\nPercentages:\n")
  print(freq_percent[[i]])
}


for (col in names(categorical_data)) {
  print(
    ggplot(categorical_data, aes_string(x = col, fill = col)) +
      geom_bar() +
      ggtitle(paste("Distribution of", col)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}