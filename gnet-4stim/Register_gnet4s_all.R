library(readr)
register_data <- read_csv("register/visual_patterns_-_register.csv")


# Select the first 11 columns
register_data <- register_data[, 1:11]

# Rename the columns with simpler names
colnames(register_data) <- c(
  "timestamp",
  "consent",
  "neptun",
  "age",
  "gender",
  "education",
  "health_issues",
  "sleep_hours",
  "coffee_time",
  "smoking_time",
  "substance_use"
)

