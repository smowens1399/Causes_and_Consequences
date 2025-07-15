################## Analysis For Winter Growth #########################
library(tidyverse)

### Perform a T-test
t.test(growth_rate~Strategy,data=growth_db)
### Calculate 95% CI for each group
calculate_ci <- function(data, confidence = 0.95) {
  n <- length(data)                     # Sample size
  mean_val <- mean(data)               # Mean of the group
  stderr <- sd(data) / sqrt(n)         # Standard error
  t_value <- qt((1 + confidence) / 2, df = n - 1) # t-critical value
  ci_lower <- mean_val - t_value * stderr
  ci_upper <- mean_val + t_value * stderr
  return(c(mean = mean_val, lower = ci_lower, upper = ci_upper))
}

# Subset data for each group
nrr_data <- growth_db$growth_rate[growth_db$Strategy == "NRR"]
dsr_data <- growth_db$growth_rate[growth_db$Strategy == "DSR"]

# Calculate CIs and range for NRR
nrr_ci <- calculate_ci(nrr_data)
nrr_min <- min(nrr_data, na.rm = TRUE)
nrr_max <- max(nrr_data, na.rm = TRUE)

# Calculate CIs and range for DSR
dsr_ci <- calculate_ci(dsr_data)
dsr_min <- min(dsr_data, na.rm = TRUE)
dsr_max <- max(dsr_data, na.rm = TRUE)

# Display results
print("NRR:")
print(nrr_ci)
cat("Min:", nrr_min, "Max:", nrr_max, "\n\n")

print("DSR:")
print(dsr_ci)
cat("Min:", dsr_min, "Max:", dsr_max, "\n")

