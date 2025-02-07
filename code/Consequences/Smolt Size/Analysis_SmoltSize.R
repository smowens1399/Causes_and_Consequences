################## Analysis For Smolt Size #########################
library(tidyverse)
### Read in smolt size data
smolt_size<-read.csv("processed_data/consequences/growth.csv")
### Perform a T-test
t.test(smoltlength~strategy,data=smolt_size)
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
### Calculate 95% CI for each group
calculate_ci <- function(data, confidence = 0.95) {
  n <- length(data)                     # Sample size
  mean_val <- mean(data, na.rm = TRUE)  # Mean of the group
  stderr <- sd(data, na.rm = TRUE) / sqrt(n) # Standard error
  t_value <- qt((1 + confidence) / 2, df = n - 1) # t-critical value
  ci_lower <- mean_val - t_value * stderr
  ci_upper <- mean_val + t_value * stderr
  return(c(mean = mean_val, lower = ci_lower, upper = ci_upper))
}

## Subset data for each group
nrr_data <- smolt_size$smoltlength[smolt_size$strategy == "NRR"]
dsr_data <- smolt_size$smoltlength[smolt_size$strategy == "DSR"]

## Calculate CIs
nrr_ci <- calculate_ci(nrr_data)
dsr_ci <- calculate_ci(dsr_data)
## Display results
print(nrr_ci)
print(dsr_ci)
