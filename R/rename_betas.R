#' rename_betas
#'
#' Renames parameters of an RJAGS to match the names of the original parameters specified in the model syntax
#'
#' @param samples Sample distributions from an RJAGS model
#' @param predictor_vars Predictor variables specified in the model syntax of the JAGSmeta function
#' @return Renamed coefficents for the model object
#' @export
#' @examples
#' rename_betas(samples, predictor_vars)



rename_betas <- function(samples, predictor_vars) {
  # Iterate over each chain in the samples
  for (chain in seq_along(samples)) {
    # Convert the current chain to a data frame for easier name manipulation
    chain_df <- as.data.frame(samples[[chain]])

    # Get the names of the current chain
    chain_names <- names(chain_df)

    # Iterate over each predictor variable to rename its beta coefficient
    for (i in seq_along(predictor_vars)) {
      beta_name <- paste0("beta[", i, "]")
      if (beta_name %in% chain_names) {
        # Rename the column in the data frame
        names(chain_df)[names(chain_df) == beta_name] <- predictor_vars[i]
      }
    }

    # Convert back to mcmc object and update the current chain in samples
    samples[[chain]] <- as.mcmc(chain_df)
  }

  return(samples)
}
