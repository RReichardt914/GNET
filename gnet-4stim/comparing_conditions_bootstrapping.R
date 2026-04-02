# Load required packages
library(lme4)

# Function to extract estimate and SE for DTF:tq interaction
extract_interaction <- function(model) {
  s <- summary(model)
  est <- s$coefficients["DTF:tqold", "Estimate"]
  se <- s$coefficients["DTF:tqold", "Std. Error"]
  return(list(estimate = est, std.error = se))
}

# Extract estimates and SEs
orig <- extract_interaction(gnet4s_orig_TLMM)
delay <- extract_interaction(gnet4s_delay_TLMM)
narr <- extract_interaction(gnet4s_artn_TLMM)
delnarr <- extract_interaction(gnet4s_delart_TLMM)

# Bootstrapped difference function
bootstrap_diff <- function(est1, se1, est2, se2, n_boot = 10000) {
  set.seed(42)
  boot1 <- rnorm(n_boot, mean = est1, sd = se1)
  boot2 <- rnorm(n_boot, mean = est2, sd = se2)
  diff <- boot1 - boot2
  ci <- quantile(diff, probs = c(0.025, 0.975))
  mean_diff <- mean(diff)
  return(list(mean = mean_diff, ci_lower = ci[1], ci_upper = ci[2]))
}

# Run comparisons
comp_orig <- bootstrap_diff(orig$estimate, orig$std.error, delay$estimate, delay$std.error)
comp_narr <- bootstrap_diff(narr$estimate, narr$std.error, delay$estimate, delay$std.error)
comp_delnarr <- bootstrap_diff(delnarr$estimate, delnarr$std.error, delay$estimate, delay$std.error)

# Print results
cat("Original vs Delay:\n")
cat(sprintf("  Mean Difference: %.4f\n  95%% CI: [%.4f, %.4f]\n\n", comp_orig$mean, comp_orig$ci_lower, comp_orig$ci_upper))

cat("Narrative vs Delay:\n")
cat(sprintf("  Mean Difference: %.4f\n  95%% CI: [%.4f, %.4f]\n\n", comp_narr$mean, comp_narr$ci_lower, comp_narr$ci_upper))

cat("Narrative + Delay vs Delay:\n")
cat(sprintf("  Mean Difference: %.4f\n  95%% CI: [%.4f, %.4f]\n", comp_delnarr$mean, comp_delnarr$ci_lower, comp_delnarr$ci_upper))
