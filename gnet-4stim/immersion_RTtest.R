library(data.table)
library(dplyr)
library(ggplot2)

# Assuming you have these data.tables and exclusion vectors:
# gnet4s_orig, gnet4s_artn, gnet4s_delay_ST, gnet4s_delart_ST
# exclude_gnet4s_orig, exclude_gnet4s_artn, exclude_gnet4s_delay, exclude_gnet4s_delart

# Function to compute average response time per participant, excluding certain IDs
compute_avg_response_time <- function(dt, exclude_ids) {
  dt[!multichar_knowresp %in% exclude_ids, 
     .(avg_response_time = mean(response_time_study_response, na.rm = TRUE)), 
     by = multichar_knowresp]
}

# Apply the function to each dataset
avg_orig <- compute_avg_response_time(gnet4s_orig, exclude_gnet4s_orig)
avg_artn <- compute_avg_response_time(gnet4s_artn, exclude_gnet4s_artn)
avg_delay <- compute_avg_response_time(gnet4s_delay_ST, exclude_gnet4s_delay)
avg_delart <- compute_avg_response_time(gnet4s_delart_ST, exclude_gnet4s_delart)

# Add a source label to each dataset
avg_orig <- avg_orig %>% mutate(source = "orig")
avg_artn <- avg_artn %>% mutate(source = "artn")
avg_delay <- avg_delay %>% mutate(source = "delay")
avg_delart <- avg_delart %>% mutate(source = "delart")

# Combine all into one data frame
combined_avg <- bind_rows(avg_orig, avg_artn, avg_delay, avg_delart) %>%
  mutate(group = case_when(
    source %in% c("orig", "delay") ~ "orig_delay",
    source %in% c("artn", "delart") ~ "artn_delart"
  ))

# Perform one-way ANOVA
anova_result <- aov(avg_response_time ~ group, data = combined_avg)
summary(anova_result)

# Optional: Tukey HSD post-hoc test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Create boxplot
ggplot(combined_avg, aes(x = source, y = avg_response_time, fill = source)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Average Response Time by Condition",
       x = "Condition", y = "Average Response Time")
