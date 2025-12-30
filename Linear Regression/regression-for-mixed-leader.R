library(readxl)
library(dplyr)

data <- read_excel("data/Questionnaire of Thesis MBA (Responses).xlsx")

data$Mixed <- ifelse(
  data$`Which type of leadership best describes the leader/manager who influences your motivation and work?` ==
    "Mixed(Combines styles depending on situation)", 
  1, 0
)


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


for(q in likert_questions) {
  data[[q]] <- as.numeric(data[[q]])
}


model_mixed <- glm(
  Mixed ~ 
    `How important is it for you to know the purpose/final goal of the project you undertake?` +
    `I feel that the work environment does not support my personal or professional development.` +
    `How important is it for you to have someone who motivates you in your work?` +
    `My leader/manager motivates me to perform at my best.` +
    `How much does micromanagement reduce your motivation at work?` +
    `The level of organization in a project significantly affects my motivation.` +
    `How does working 37 hours a week influence your motivation?` +
    `Do you feel fear of being laid off at your current job`,
  data = data,
  family = binomial
)

summary(model_mixed)


odds_ratios <- exp(coef(model_mixed))
conf_int <- exp(confint(model_mixed))

odds_ratios
conf_int

z_values <- summary(model_mixed)$coefficients[, "Estimate"] /
  summary(model_mixed)$coefficients[, "Std. Error"]

p_values <- 2 * (1 - pnorm(abs(z_values)))
p_values


predicted_probs <- predict(model_mixed, type = "response")
head(predicted_probs)
