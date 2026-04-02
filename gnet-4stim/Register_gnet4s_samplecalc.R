library(dplyr)
library(stringr)

# Combine all exclusion vectors into one (case-insensitive)
all_excludes <- tolower(c(exclude_gnet4s_orig, exclude_gnet4s_artn, exclude_gnet4s_delay, exclude_gnet4s_delart))

# Combine all multichar_knowresp values from the four tibbles, convert to lowercase, and exclude unwanted values
all_neptun_ids <- unique(tolower(c(
  gnet4s_orig_StudyCR$multichar_knowresp,
  gnet4s_delay_StudyCR$multichar_knowresp,
  gnet4s_artn_StudyCR$multichar_knowresp,
  gnet4s_delart_StudyCR$multichar_knowresp
)))

# Filter out excluded values
valid_neptun_ids <- setdiff(all_neptun_ids, all_excludes)

# Filter register_data with case-insensitive matching
matched_data <- register_data %>%
  mutate(neptun_lower = tolower(neptun)) %>%
  filter(neptun_lower %in% valid_neptun_ids)

# Calculate statistics
mean_age <- mean(matched_data$age, na.rm = TRUE)
sd_age <- sd(matched_data$age, na.rm = TRUE)
num_females <- sum(matched_data$gender == "Nő", na.rm = TRUE)

# Print results
cat("Mean age:", mean_age, "\n")
cat("Standard deviation of age:", sd_age, "\n")
cat("Number of females (Nő):", num_females, "\n")

