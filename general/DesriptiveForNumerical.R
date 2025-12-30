library(psych)
library(readxl)
library(dplyr)
data <- read_excel("data/Questionnaire of Thesis MBA (Responses).xlsx")
numeric_data <- data %>%
  select(where(is.numeric))
describe(numeric_data)
