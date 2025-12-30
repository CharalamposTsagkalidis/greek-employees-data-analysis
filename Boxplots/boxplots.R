library(readxl)
library(dplyr)
library(ggplot2)


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


likert_data$Experience_Group <- ifelse(
  likert_data$Experience %in% c("1 year", "2 years", "3 years"),
  "1-3 years",
  "Other"
)

likert_data$Experience_Group <- factor(likert_data$Experience_Group)


make_boxplot <- function(df, variable, y_label) {
  ggplot(df, aes(x = Experience_Group, y = .data[[variable]], fill = Experience_Group)) +
    geom_boxplot(alpha = 0.7) +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("Boxplot of", y_label, "by Experience Group"),
      x = "Experience Group",
      y = y_label
    ) +
    scale_fill_brewer(palette = "Set2")
}


p1 <- make_boxplot(likert_data, "Purpose_Leadership", "Purpose & Leadership")
p2 <- make_boxplot(likert_data, "Org_Environment", "Organizational Environment")
p3 <- make_boxplot(likert_data, "External_Stress", "External Stress")


print(p1)
print(p2)
print(p3)
