#' rename_betas
#'
#' Renames parameters of an RJAGS to match the names of the original parameters specified in the model syntax
#'
#' @param samples Sample distributions from an RJAGS model
#' @param predictor_vars Predictor variables specified in the model syntax of the JAGSmeta function
#' @return Renamed coefficients for the model object
#' @export
#' @examples
#' \dontrun{
#' rename_betas(samples, predictor_vars)
#' }


rename_betas <- function(samples, predictor_vars) {
  var_names <- varnames(samples)
  beta_names <- grep("^beta\\[", var_names, value = TRUE)
  for (i in seq_along(beta_names)) {
    var_names[var_names == beta_names[i]] <- predictor_vars[i]
  }
  varnames(samples) <- var_names
  return(samples)
}


