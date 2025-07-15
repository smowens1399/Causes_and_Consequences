##################### Calculate QAIC, model Weight and R-squared deviance for all models ############################
########QAIC########
## QAIC Function
calculate_QAIC <- function(vars) {
  formula <- as.formula(paste("cbind(Total_DSR, Total_NRR) ~", paste(vars, collapse = " + ")))
  model_binomial <- glm(formula, family = binomial(link = "logit"), data = model_inputs)
  model_quasibinomial <- glm(formula, family = quasibinomial(link = "logit"), data = model_inputs)
  residual_deviance <- deviance(model_quasibinomial)
  residual_df <- df.residual(model_quasibinomial)
  dispersion_parameter <- residual_deviance / residual_df
  loglike <- as.numeric(logLik(model_binomial))
  num_parameters <- length(coef(model_quasibinomial))
  QAIC <- ((-2 * loglike) / dispersion_parameter) + (2 * num_parameters)
  return(QAIC)
}
## Calculate QAIC for each combination of variables
QAIC_values <- sapply(iv_combos, calculate_QAIC)
## Put QAIC results into a dataframe
model_combos <- sapply(iv_combos, function(vars) paste(vars, collapse = " + "), simplify = FALSE)
model_ranking <- data.frame(Combo = unlist(model_combos), QAIC = QAIC_values)
############ Model Weights ###################
##Function for model weights
calculate_model_weights <- function(QAIC) {
  delta_qaic <- QAIC - min(QAIC)  # Delta QAIC
  weights <- exp(-0.5 * delta_qaic) / sum(exp(-0.5 * delta_qaic))  # Model weights
  return(weights)
}
## Calculate Delta QAIC and Model Weights and add to qaic db
model_ranking$Delta_QAIC <- model_ranking$QAIC - min(model_ranking$QAIC)  # Delta QAIC
model_ranking$Weight <- calculate_model_weights(model_ranking$QAIC)      # Model weights
########## Deviance R²#################
## Deviance R² function
calculate_deviance_r2 <- function(vars) {
  formula <- as.formula(paste("cbind(Total_DSR, Total_NRR) ~", paste(vars, collapse = " + ")))
  model_quasibinomial <- glm(formula, family = quasibinomial(link = "logit"), data = model_inputs)
  null_model <- glm(cbind(Total_DSR, Total_NRR) ~ 1, family = quasibinomial(link = "logit"), data = model_inputs)
  deviance_fitted <- deviance(model_quasibinomial)
  deviance_null <- deviance(null_model)
  r2_deviance <- 1 - (deviance_fitted / deviance_null)
  return(r2_deviance)
}
## Calculate Deviance R² for each combination of variables
Deviance_R2_values <- sapply(iv_combos, calculate_deviance_r2)
## Add Deviance R² to the dataframe
model_ranking$Deviance_R2 <- Deviance_R2_values
