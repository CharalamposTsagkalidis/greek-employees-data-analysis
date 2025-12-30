

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



likert_data$Experience <- factor(
  likert_data$Experience,
  levels = c(
    "Less than 1 year",
    "1–3 years",
    "4–6 years",
    "7–10 years",
    "More than 10 years"
  ),
  ordered = TRUE
)



likert_data$MicromanagementEffect <- 6 - likert_data$MicromanagementEffect
likert_data$EnvironmentSupport   <- 6 - likert_data$EnvironmentSupport



likert_data <- likert_data %>%
  mutate(
    Purpose_Leadership = rowMeans(select(., PurposeImportance, MotivationImportance, LeaderMotivation), na.rm = TRUE),
    Org_Environment    = rowMeans(select(., EnvironmentSupport, ProjectOrganization), na.rm = TRUE),
    External_Stress    = rowMeans(select(., MicromanagementEffect, WorkHoursEffect, FearLayoff), na.rm = TRUE)
  )



t_exp_purpose <- t.test(
  Purpose_Leadership ~ Experience,
  data = likert_data,
  subset = Experience %in% c("Less than 1 year", "More than 10 years")
)

t_exp_env <- t.test(
  Org_Environment ~ Experience,
  data = likert_data,
  subset = Experience %in% c("Less than 1 year", "More than 10 years")
)

t_exp_stress <- t.test(
  External_Stress ~ Experience,
  data = likert_data,
  subset = Experience %in% c("Less than 1 year", "More than 10 years")
)

cat("--------------------------------------------------------\n")
cat("T-test: Purpose & Leadership (Less than 1 vs >10 years)\n")
print(t_exp_purpose)

cat("\n--------------------------------------------------------\n")
cat("T-test: Organizational Environment (Less than 1 vs >10 years)\n")
print(t_exp_env)

cat("\n--------------------------------------------------------\n")
cat("T-test: External Stress (Less than 1 vs >10 years)\n")
print(t_exp_stress)


anova_purpose <- aov(Purpose_Leadership ~ Experience, data = likert_data)
anova_env     <- aov(Org_Environment ~ Experience, data = likert_data)
anova_stress  <- aov(External_Stress ~ Experience, data = likert_data)

cat("\n--------------------------------------------------------\n")
cat("ANOVA: Purpose & Leadership across all Experience groups\n")
summary(anova_purpose)

cat("\n--------------------------------------------------------\n")
cat("ANOVA: Organizational Environment across all Experience groups\n")
summary(anova_env)

cat("\n--------------------------------------------------------\n")
cat("ANOVA: External Stress across all Experience groups\n")
summary(anova_stress)



kw_purpose <- kruskal.test(Purpose_Leadership ~ Experience, data = likert_data)
kw_env     <- kruskal.test(Org_Environment ~ Experience, data = likert_data)
kw_stress  <- kruskal.test(External_Stress ~ Experience, data = likert_data)

cat("\n--------------------------------------------------------\n")
cat("Kruskal–Wallis: Purpose & Leadership across Experience groups\n")
print(kw_purpose)

cat("\n--------------------------------------------------------\n")
cat("Kruskal–Wallis: Organizational Environment across Experience groups\n")
print(kw_env)

cat("\n--------------------------------------------------------\n")
cat("Kruskal–Wallis: External Stress across Experience groups\n")
print(kw_stress)
