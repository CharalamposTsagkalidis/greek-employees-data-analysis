library(readxl)
library(dplyr)
library(psych)

# Load dataset
data <- read_excel("data/Questionnaire of Thesis MBA (Responses).xlsx")



likert_data <- data %>%
  select(
    `Please indicate your biological sex as assigned at birth:`,
    `How important is it for you to know the purpose/final goal of the project you undertake?`,
    `I feel that the work environment does not support my personal or professional development.`,
    `How important is it for you to have someone who motivates you in your work?`,
    `My leader/manager motivates me to perform at my best.`,
    `How much does micromanagement reduce your motivation at work?`,
    `The level of organization in a project significantly affects my motivation.`,
    `How does working 37 hours a week influence your motivation?`,
    `Do you feel fear of being laid off at your current job`
  )

names(likert_data) <- c(
  "Gender", "PurposeImportance", "EnvironmentSupport", "MotivationImportance",
  "LeaderMotivation", "MicromanagementEffect", "ProjectOrganization",
  "WorkHoursEffect", "FearLayoff"
)


likert_data$MicromanagementEffect <- 6 - likert_data$MicromanagementEffect
likert_data$EnvironmentSupport <- 6 - likert_data$EnvironmentSupport


likert_data <- likert_data %>%
  mutate(
    Purpose_Leadership = rowMeans(select(., PurposeImportance, MotivationImportance, LeaderMotivation), na.rm = TRUE),
    Org_Environment = rowMeans(select(., EnvironmentSupport, ProjectOrganization), na.rm = TRUE),
    External_Stress = rowMeans(select(., MicromanagementEffect, WorkHoursEffect, FearLayoff), na.rm = TRUE)
  )


likert_data$Gender <- as.factor(likert_data$Gender)


t1 <- t.test(Purpose_Leadership ~ Gender, data = likert_data)
t2 <- t.test(Org_Environment ~ Gender, data = likert_data)
t3 <- t.test(External_Stress ~ Gender, data = likert_data)


cat("T-test for Purpose & Leadership:\n")
print(t1)
cat("\nT-test for Organizational Environment:\n")
print(t2)
cat("\nT-test for External & Stress Factors:\n")
print(t3)
