library(psych)


library(readxl)
library(dplyr)

data <- read_excel("data/Questionnaire of Thesis MBA (Responses).xlsx")

# --- Step 3: Select only Likert-scale numeric questions ---
likert_data <- data %>%
  select(
    `How important is it for you to know the purpose/final goal of the project you undertake?`,
    `I feel that the work environment does not support my personal or professional development.`,
    `How important is it for you to have someone who motivates you in your work?`,
    `My leader/manager motivates me to perform at my best.`,
    `How much does micromanagement reduce your motivation at work?`,
    `The level of organization in a project significantly affects my motivation.`,
    `How does working 37 hours a week influence your motivation?`,
    `Do you feel fear of being laid off at your current job`
  )

# --- Step 4: Compute Cronbach’s Alpha ---
alpha_result <- psych::alpha(likert_data)

# --- Step 5: View results ---
print(alpha_result)