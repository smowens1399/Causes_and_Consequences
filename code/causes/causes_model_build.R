#################### Model combinations ############################
library(car)
### Generate all possilbe combinations of independent variables with select interactions  
# Define independent variables
independent_variables <- c("Qvar", "DD", "BM", "Qcum", "Den")
# Define interaction terms
interactions <- c("Den:BM", "Den:DD","BM:DD")
# Combine independent variables and interaction terms
all_variables <- c(independent_variables, interactions)
# Generate all possible combinations of independent variables
iv_combos <- unlist(lapply(1:length(all_variables), function(i) combn(all_variables, i, simplify = FALSE)), recursive = FALSE)

### Fit models to combinations of variables
model_combos <- lapply(iv_combos, function(vars) {
  # Create formula for the model
  formula <- as.formula(paste("cbind(Total_DSR, Total_NRR) ~", paste(vars, collapse = " + ")))
  # Fit the model
  glm(formula, family = quasibinomial(link = "logit"), data = model_inputs)
})



### Check for multi-colinearity
# Example for one model
model_vif <- glm(cbind(Total_DSR, Total_NRR) ~ Qvar + DD + BM + Den , 
             family = quasibinomial, data = model_inputs)

vif(model_vif)
