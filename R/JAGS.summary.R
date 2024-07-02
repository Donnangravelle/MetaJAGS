#' JAGS.summary
#'
#' Creates a simplified model summary output
#'
#' @param mcmc_samples A RJAGS model Object
#' @return A simplified model summary that excludes estimates at the study level
#' @importFrom coda mcmc mcmc.list
#' @export
#' @examples
#' \dontrun{
#'
#' JAGS.summary(JAGSmeta_model)
#'
#' }
JAGS.summary <- function(mcmc_samples) {
  # Initialize an empty list to store the filtered chains
  filtered_samples <- list()

  # Loop through each chain in the MCMC list
  for (i in seq_along(mcmc_samples)) {
    # Convert the individual chain to a matrix
    chain_matrix <- as.matrix(mcmc_samples[[i]])

    # Identify and exclude variables starting with 'alpha'
    include_vars <- grep("^alpha", colnames(chain_matrix), invert = TRUE, value = TRUE)

    # Filter the chain matrix to exclude these variables
    filtered_chain_matrix <- chain_matrix[, include_vars]

    # Convert back to an mcmc object for each chain
    filtered_samples[[i]] <- mcmc(filtered_chain_matrix, start = 1, end = nrow(filtered_chain_matrix), thin = 1)
  }

  # Combine the filtered chains back into an mcmc.list
  filtered_samples <- do.call(mcmc.list, filtered_samples)

  # Generate and return the summary of the filtered MCMC list
  return(summary(filtered_samples))
}
