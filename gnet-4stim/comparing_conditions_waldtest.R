# Function to extract estimate and SE for DTF:tq interaction
extract_coef <- function(model) {
  coefs <- summary(model)$coefficients
  est <- coefs["DTF:tqold", "Estimate"]
  se <- coefs["DTF:tqold", "Std. Error"]
  return(c(est, se))
}

# Extract estimates and SEs
orig <- extract_coef(gnet4s_orig_TLMM)
delay <- extract_coef(gnet4s_delay_TLMM)
narr <- extract_coef(gnet4s_artn_TLMM)
delart <- extract_coef(gnet4s_delart_TLMM)

# Wald test function
wald_test <- function(est1, se1, est2, se2) {
  z <- (est1 - est2) / sqrt(se1^2 + se2^2)
  p <- 2 * (1 - pnorm(abs(z)))
  return(data.frame(z = z, p = p))
}

# Run comparisons
test_orig_vs_delay <- wald_test(orig[1], orig[2], delay[1], delay[2])
test_narr_vs_delay <- wald_test(narr[1], narr[2], delay[1], delay[2])
test_delart_vs_delay <- wald_test(delart[1], delart[2], delay[1], delay[2])

# Print results
cat("Original vs Delay:\n")
print(test_orig_vs_delay)

cat("\nNarrative vs Delay:\n")
print(test_narr_vs_delay)

cat("\nNarrative + Delay vs Delay:\n")
print(test_delart_vs_delay)
