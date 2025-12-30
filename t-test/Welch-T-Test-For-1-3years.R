library(readxl)
library(dplyr)
library(psych)


data <- read_excel("data/Questionnaire of Thesis MBA (Responses).xlsx")

likert_data <- data %>%
  select(
    `How many years of full-time professional work experience do you have:`,
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
  "Experience", "PurposeImportance", "EnvironmentSupport", "MotivationImportance",
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

unique(likert_data$Experience)


likert_data$Experience_Group <- ifelse(
  likert_data$Experience %in% c("1 year", "2 years", "3 years", "1–3 years", "1 – 3 years"),
  "1-3 years",
  "Other"
)

likert_data$Experience_Group <- as.factor(likert_data$Experience_Group)


table(likert_data$Experience_Group, useNA = "ifany")


likert_clean <- likert_data %>%
  filter(!is.na(Experience_Group))

t1 <- t.test(Purpose_Leadership ~ Experience_Group, data = likert_clean)
t2 <- t.test(Org_Environment ~ Experience_Group, data = likert_clean)
t3 <- t.test(External_Stress ~ Experience_Group, data = likert_clean)


cat("T-test: Purpose & Leadership (1–3 years vs Others)\n")
print(t1)

cat("\nT-test: Organizational Environment (1–3 years vs Others)\n")
print(t2)

cat("\nT-test: External Stress Factors (1–3 years vs Others)\n")
print(t3)
