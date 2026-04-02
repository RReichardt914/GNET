# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(stats)

# Filter and label each tibble
orig_filtered <- gnet4s_orig_StudyCR %>%
  filter(!multichar_knowresp %in% exclude_gnet4s_orig) %>%
  mutate(source = "orig")

artn_filtered <- gnet4s_artn_StudyCR %>%
  filter(!multichar_knowresp %in% exclude_gnet4s_artn) %>%
  mutate(source = "artn")

delay_filtered <- gnet4s_delay_StudyCR %>%
  filter(!multichar_knowresp %in% exclude_gnet4s_delay) %>%
  mutate(source = "delay")

delart_filtered <- gnet4s_delart_StudyCR %>%
  filter(!multichar_knowresp %in% exclude_gnet4s_delart) %>%
  mutate(source = "delart")

# Combine all filtered data
combined_FA <- bind_rows(orig_filtered, artn_filtered, delay_filtered, delart_filtered) %>%
  select(FA, source) %>%
  filter(!is.na(FA)) %>%
  mutate(group = case_when(
    source %in% c("orig", "delay") ~ "orig_delay",
    source %in% c("artn", "delart") ~ "artn_delart"
  ))

# Perform one-way ANOVA on grouped data
anova_result <- aov(FA ~ group, data = combined_FA)
summary(anova_result)

# Optional: Tukey HSD post-hoc test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Create boxplot
ggplot(combined_FA, aes(x = group, y = FA, fill = group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "FA Comparison: orig+delay vs artn+delart",
       x = "Group", y = "FA")
