########################### Summarize top model_combos ##################
model_summary <- function(index) {
  vars <- iv_combos[[index]]
  formula <- as.formula(paste("cbind(Total_DSR, Total_NRR) ~", paste(vars, collapse = " + ")))
  model <- glm(formula, family = quasibinomial(link = "logit"), data = model_inputs)
  summary(model)
}

# Example usage:
model_summary(6)
