################## Analysis For Time of Ocean Entry #########################
library(tidyverse)
library(lmerTest)
library(lme4)

### Perform T-Test
t.test(detection_doy~Strategy,data=ocean_entry)
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

## Subset data for each group
nrr_data <- ocean_entry$detection_doy[ocean_entry$Strategy == "NRR"]
dsr_data <- ocean_entry$detection_doy[ocean_entry$Strategy == "DSR"]

## Calculate CIs
nrr_ci <- calculate_ci(nrr_data)
dsr_ci <- calculate_ci(dsr_data)
## Display results
print(nrr_ci)
print(dsr_ci)


