#' JAGSmeta
#'
#' Function for estimating a hierarchical Bayesian Meta-regression using RJAGS
#'
#' @param formula Model formula that specifies the model
#' @param v Variance variable in data frame
#' @param studyID Column containing the study IDs that effects will be nested in
#' @param data Data frame containing the variables used in the model
#' @param n_chains Number of chains used to generate samples
#' @param n_updates Number of updates used to generate samples
#' @param n_iterations Number of iterations used to generate samples
#' @param prob_conditions List of probability statements to be estimated for specified parameters
#' @return MCMC Graphical model
#' @importFrom rjags jags.model coda.samples
#' @importFrom dplyr mutate
#' @export
#' @examples
#' \dontrun{
#' result <- JAGSmeta(
#' formula = yi ~ 1 + moderator_1 + moderator_2, # Model formula
#' v = "vi", # Column name for variance
#' studyID = "study_id", # Column name for study identifier
#' data = example_data, # The dataset
#' n_chains = 5, # Number of MCMC chains
#' n_updates = 100000, # Number of updates for burn-in
#' n_iterations = 100000 # Number of iterations for sampling
#'  prob_conditions = list(
#' "mu.alpha" = list(
#'   "greater" =c(0,.2, .5, .8)),
#' "sd.alpha" = list(
#'   "lesser" = .1,
#'   "between" = list(c(.1,.2), c(.2,.5)),
#'   "greater" = .5)
#'   )
#' )
#' }
JAGSmeta <- function(formula, v, studyID, data, n_chains = 3, n_updates = 10000, n_iterations = 10000, prob_conditions = NULL) {
  data$identifier <- as.numeric(as.factor(data[[studyID]]))

  response_var <- all.vars(formula)[1]
  predictor_vars <- all.vars(formula)[-1]
  predictor_vars <- predictor_vars[predictor_vars != "1"] # Remove intercept if specified explicitly

  dat_jags <- list(
    "study" = data$identifier,
    "ES" = data[[response_var]],
    "Var" = data[[v]],
    "n_effects" = nrow(data),
    "n_studies" = length(unique(data$identifier))
  )
  for (predictor in predictor_vars) {
    dat_jags[[predictor]] <- data[[predictor]]
  }

  model_string <- "model {\n"
  model_string <- paste0(model_string, "  mu.alpha ~ dnorm(0, 1)\n")
  model_string <- paste0(model_string, "  sd.alpha ~ dunif(0, 3)\n")
  model_string <- paste0(model_string, "  tau.alpha <- pow(sd.alpha, -2)\n")
  model_string <- paste0(model_string, "  sd.err ~ dunif(0, 3)\n")
  model_string <- paste0(model_string, "  prec.err <- pow(sd.err, -2)\n")

  # Insert global beta definitions here if there are predictors
  if (length(predictor_vars) > 0) {
    model_string <- paste0(model_string, "  for (k in 1:", length(predictor_vars), ") {\n    beta[k] ~ dnorm(0, 1)\n  }\n")
  }

  model_string <- paste0(model_string, "  for (i in 1:n_effects) {\n")
  model_string <- paste0(model_string, "    ES[i] ~ dnorm(mu[i], tau[i])\n")
  model_string <- paste0(model_string, "    mu[i] <- alpha[study[i]]")

  if (length(predictor_vars) > 0) {
    for (i in 1:length(predictor_vars)) {
      model_string <- paste0(model_string, " + beta[", i, "] * ", predictor_vars[i], "[i]")
    }
  }

  model_string <- paste0(model_string, " + err[i]\n")
  model_string <- paste0(model_string, "    err[i] ~ dnorm(0, prec.err)\n")
  model_string <- paste0(model_string, "    tau[i] <- 1/Var[i]\n  }\n")

  model_string <- paste0(model_string, "  for (j in 1:n_studies) {\n    alpha[j] ~ dnorm(mu.alpha, tau.alpha)\n  }\n")

  if (!is.null(prob_conditions)) {
    model_string <- add_probability_statements(model_string, prob_conditions, predictor_vars)
  }

  model_string <- paste0(model_string, "}")

  jags_model <- jags.model(textConnection(model_string), data = dat_jags, n.chains = n_chains)
  update(jags_model, n_updates)

  sample_variables <- get_sample_variables(predictor_vars, prob_conditions)
  samples <- coda.samples(jags_model, variable.names = sample_variables, n.iter = n_iterations, n.chains=n_chains)

  renamed_samples <- rename_betas(samples, predictor_vars)
  return(renamed_samples)
}



get_sample_variables <- function(predictor_vars, prob_conditions) {
  sample_variables <- c("mu.alpha", "sd.alpha", "sd.err", "alpha")

  if (length(predictor_vars) > 0) {
    sample_variables <- c(sample_variables, paste0("beta[", seq_along(predictor_vars), "]"))
  }

  if (!is.null(prob_conditions)) {
    for (param in names(prob_conditions)) {
      conditions <- prob_conditions[[param]]
      for (condition_type in names(conditions)) {
        thresholds <- conditions[[condition_type]]
        if (condition_type == "between") {
          for (range in thresholds) {
            low_name = ifelse(range[1] < 0, paste("neg", gsub("\\.", "p", format(abs(range[1]), nsmall = 2)), sep=""), gsub("\\.", "p", format(range[1], nsmall = 2)))
            high_name = ifelse(range[2] < 0, paste("neg", gsub("\\.", "p", format(abs(range[2]), nsmall = 2)), sep=""), gsub("\\.", "p", format(range[2], nsmall = 2)))
            sample_variables <- c(sample_variables, sprintf("%s.between%sAnd%s", param, low_name, high_name))
          }
        } else {
          for (threshold in thresholds) {
            if (threshold < 0 && condition_type == "lesser") {
              sample_variables <- c(sample_variables, sprintf("%s.lesserNeg%.2f", param, abs(threshold)))
            } else {
              sample_variables <- c(sample_variables, sprintf("%s.%s%.2f", param, condition_type, threshold))
            }
          }
        }
      }
    }
  }

  return (sample_variables)
}

