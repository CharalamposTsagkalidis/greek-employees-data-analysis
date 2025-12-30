library(ggplot2)
library(psych)
library(readxl)
library(dplyr)
data <- read_excel("data/Questionnaire of Thesis MBA (Responses).xlsx")
numeric_data <- data %>%
  select(where(is.numeric))
names(numeric_data) <- c(
  "PurposeImportance",
  "EnvironmentSupport",
  "MotivationImportance",
  "LeaderMotivation",
  "MicromanagementEffect",
  "ProjectOrganization",
  "WorkHoursEffect",
  "FearLayoff"
)
for (col in names(numeric_data)) {
  print(
    ggplot(numeric_data, aes_string(x = col)) +
      geom_histogram(binwidth = 1, fill="steelblue", color="white") +
      ggtitle(paste("Histogram of", col))
  )
}