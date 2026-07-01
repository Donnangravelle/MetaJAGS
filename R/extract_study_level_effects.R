#' extract_study_level_effects
#'
#' Extracts the study level intercepts and prepares them in a dataframe
#'
#' @param model RJAGS Model
#' @param study_info Column with study labels
#' @param data Data frame with origianl data
#' @return Data frame of study level estimates
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter rename mutate summarise
#' @importFrom dplyr starts_with %>% everything
#' @export
#' @examples
#' \dontrun{
#' extract_study_level_effects(model, study_info = "Study_Info",
#' data = example_data)
#' }
#'
#'
#'
#'
#'
extract_study_level_effects <- function(model,study_info,data){
  # Extract 'mu.alpha' samples
  print("Extracting Samples")
  mu_alpha_samples <- sapply(model, function(chain) {
    as.matrix(chain)[, "mu.alpha"]
  }, simplify = "array")


  # Convert to dataframe and reshape
  mu_alpha_df <- as.data.frame(mu_alpha_samples)
  colnames(mu_alpha_df) <- paste0("mu_alpha_values_", seq_along(colnames(mu_alpha_df)))
  mu_alpha_long <- pivot_longer(mu_alpha_df, cols = starts_with("mu_alpha_values"), names_to = "chain", values_to = "mu_alpha_values")

  # Extract alpha samples
  alpha_samples <- lapply(model, function(chain) {
    chain_data <- as.matrix(chain)
    alpha_columns <- grep("alpha\\[", colnames(chain_data))
    chain_data[, alpha_columns, drop = FALSE]
  })

  # Reshape alpha samples for plotting
  alpha_samples_df <- do.call(rbind, alpha_samples) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "study", values_to = "alpha_value") %>%
    mutate(study = as.factor(gsub("alpha\\[(\\d+)\\]", "\\1", study)))

  # Merge and finalize datasets
  mu_alpha_long_renamed <- mu_alpha_long %>%
    rename(study = chain, alpha_value = mu_alpha_values) %>%
    mutate(study = "mu")

  full_alpha <- rbind(mu_alpha_long_renamed, alpha_samples_df)
  identifier <-  as.numeric(as.factor(data[[study_info]]))

  study_mapping <- setNames(data[[study_info]], identifier)


  full_alpha$study <- factor(study_mapping[as.character(full_alpha$study)],
                             levels = c("mu", sort(unique(study_mapping), decreasing = TRUE)))
  full_alpha$study[is.na(full_alpha$study)] <- "mu"
  levels(full_alpha$study)[levels(full_alpha$study) == "mu"] <- "\u03BC" # Unicode for Greek letter mu


  # Calculate point estimates
  print("Calculating Point Estimates")
  point_estimates <- aggregate(alpha_value ~ study, data = full_alpha, FUN = approximate_mode)
  mu_estimate <- point_estimates %>% filter(study == "\u03BC")
  # Summary for plotting
  summary_df <- full_alpha %>%
    group_by(study) %>%
    summarise(
      mean_alpha = approximate_mode(alpha_value),
      lower_ci = quantile(alpha_value, probs = 0.025),
      upper_ci = quantile(alpha_value, probs = 0.975)
    )
  return(summary_df)
}





