library(readxl)
library(dplyr)
library(ggplot2)

data <- read_excel("data/Questionnaire of Thesis MBA (Responses).xlsx")


long_categorical <- data %>%
  select(
    `Which type of leadership best describes the leader/manager who influences your motivation and work?`,
    `What is the long-term purpose or vision that drives you in your work?`,
    `When you feel a lack of motivation in your job, which of the following responses best describes how you typically react?`,
    `Which of the following motivates you the most in your work  ?`
  )


names(long_categorical) <- c(
  "LeadershipType",
  "LongTermPurpose",
  "ReactionToLowMotivation",
  "WorkMotivation"
)


long_categorical <- long_categorical %>%
  mutate(
    LeadershipType = case_when(
      grepl("Transformational", LeadershipType, ignore.case = TRUE),
      grepl("Democratic", LeadershipType, ignore.case = TRUE),
      grepl("Autocratic", LeadershipType, ignore.case = TRUE),
      TRUE ~ "Other"
    ),
    LongTermPurpose = case_when(
      grepl("Career", LongTermPurpose, ignore.case = TRUE),
      grepl("Money|Salary", LongTermPurpose, ignore.case = TRUE) ,
      grepl("Learning|Skill", LongTermPurpose, ignore.case = TRUE),
      TRUE ~ "Other"
    ),
    ReactionToLowMotivation = case_when(
      grepl("Feedback|Talk|Discuss", ReactionToLowMotivation, ignore.case = TRUE),
      grepl("Break|Rest|Pause", ReactionToLowMotivation, ignore.case = TRUE),
      grepl("Ignore|Continue", ReactionToLowMotivation, ignore.case = TRUE),
      TRUE ~ "Other"
    )
  
  )


for (col in names(long_categorical)) {
  cat("\n==============================\n")
  cat("Question:", col, "\n")
  cat("==============================\n")
  
  freq_table <- long_categorical %>%
    count(!!sym(col)) %>%
    mutate(Percentage = round(100 * n / sum(n), 1)) %>%
    arrange(desc(n))
  
  print(freq_table)
}


for (col in names(long_categorical)) {
  p <- ggplot(long_categorical, aes(x = .data[[col]])) +
    geom_bar(fill = "steelblue") +
    coord_flip() +
    theme_minimal(base_size = 12) +
    labs(title = paste("Responses for:", col), x = "", y = "Frequency") +
    theme(plot.title = element_text(face = "bold"))
  
  print(p)
}