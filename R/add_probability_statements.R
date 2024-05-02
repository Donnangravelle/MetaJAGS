#' add_probability_statements
#'
#' Responsible for adding probability statements to the JAGSmeta model
#'
#' @param model_string Model string specifying the parameters to be estimated
#' @param prob_conditions Probabilities to be estimated for the parameters
#' @param predictor_vars Predictor variables specifying in the model formula
#' @return Probability statements to be included in the model string
#' @export
#' @examples
#' \dontrun{
#' add_probability_statements(model_string, prob_conditions, predictor_vars)
#' }




add_probability_statements <- function(model_string, prob_conditions, predictor_vars) {
  for (param in names(prob_conditions)) {
    conditions <- prob_conditions[[param]]
    for (condition_type in names(conditions)) {
      thresholds = conditions[[condition_type]]
      if (condition_type == "between") {
        for (range in thresholds) {
          low_name = ifelse(range[1] < 0, paste("neg", gsub("\\.", "p", format(abs(range[1]), nsmall = 2)), sep=""), gsub("\\.", "p", format(range[1], nsmall = 2)))
          high_name = ifelse(range[2] < 0, paste("neg", gsub("\\.", "p", format(abs(range[2]), nsmall = 2)), sep=""), gsub("\\.", "p", format(range[2], nsmall = 2)))
          prob_var_name = sprintf("%s.between%sAnd%s", param, low_name, high_name)
          if (param %in% predictor_vars) {
            # Ensure this is applied to the global beta coefficients
            model_string <- paste0(model_string, "  ", prob_var_name, " <- step(beta[", which(predictor_vars == param), "] - ", range[1], ") * step(", range[2], " - beta[", which(predictor_vars == param), "])\n")
          } else {
            # Apply to global parameters not associated with specific effects
            model_string <- paste0(model_string, "  ", prob_var_name, " <- step(", param, " - ", range[1], ") * step(", range[2], " - ", param, ")\n")
          }
        }
      } else {
        for (threshold in thresholds) {
          prob_var_name = if (threshold < 0 && condition_type == "lesser") {
            sprintf("%s.lesserNeg%.2f", param, abs(threshold))
          } else {
            sprintf("%s.%s%.2f", param, condition_type, threshold)
          }
          if (param %in% predictor_vars) {
            model_string <- paste0(model_string, "  ", prob_var_name, " <- step(beta[", which(predictor_vars == param), "] - ", threshold, ")\n")
          } else {
            model_string <- paste0(model_string, "  ", prob_var_name, " <- step(", param, " - ", threshold, ")\n")
          }
        }
      }
    }
  }
  return (model_string)
}

