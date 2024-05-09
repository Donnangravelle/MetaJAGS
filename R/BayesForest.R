#' BayesForest
#'
#' Creates forest plot of effect size estimates at the study level with distributions
#'
#' @param model RJAGS Model
#' @param study_info Column with study labels
#' @param data Data frame with origianl data
#' @param dens_color Color of density curves
#' @param point_color Color of point estimates
#' @param CI_offset Distance of critical intervals from density curves
#' @param overlap Overlap between study density curves
#' @param trim_percentile The limits of the density curves
#' @param y_text_size Size of study labels
#' @param y_title_size Size of y-axis label
#' @param CI_size Size of critical interval text
#' @param x_text_size Size of x-axis tick text
#' @param x_title_size Size of x-axis title text
#' @return Forest plots of density curves
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter rename mutate summarise
#' @importFrom ggplot2 ggplot geom_point geom_vline labs theme_minimal theme element_text
#' @importFrom ggridges geom_density_ridges_gradient
#' @importFrom dplyr starts_with %>% everything
#' @export
#' @examples
#' \dontrun{
#' BayesForest(model, study_info = "Study_Info",
#' data = example_data, dens_color = "skyblue",
#' point_color = "black", overlap = 1,
#' CI_offset = .3, y_text_size = 14, CI_size = 5, x_text_size = 14,
#' x_title_size = 14, y_title_size = 14)
#' }
#'
#'
#'
#'
#'
BayesForest <- function(model,study_info, data, dens_color = "skyblue",point_color = "black",
                        CI_offset = .75, overlap = 1, trim_percentile = 0.01,
                        y_text_size = 12, y_title_size = 12, CI_size = 12,
                        x_text_size = 12, x_title_size = 12){
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

  lower_bound <- quantile(full_alpha$alpha_value, probs = trim_percentile)
  upper_bound <- quantile(full_alpha$alpha_value, probs = 1 - trim_percentile)
  full_alpha_trimmed <- full_alpha %>%
    filter(alpha_value >= lower_bound, alpha_value <= upper_bound)

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



  # Plot
  print("Plotting...")
  ggplot(full_alpha_trimmed, aes(x = alpha_value, y = study)) +
    geom_density_ridges_gradient(scale = overlap, fill = dens_color) +
    geom_point(data = point_estimates, aes(x = alpha_value), color = point_color, size = 2, shape = 16) +
    geom_vline(xintercept = mean(mu_estimate$alpha_value), linetype = "solid", color = "gray", size = 1, alpha = .75) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black") +
    labs(x = "Standardized Mean Difference", y = "Study Information") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(family = "Times New Roman", face = "bold"),
          plot.title = element_text(family = "Times New Roman", face = "bold"),
          axis.title = element_text(family = "Times New Roman"),
          axis.text = element_text(family = "Times New Roman"),
          axis.text.y = element_text(size = y_text_size),
          axis.title.y = element_text(size = y_title_size),
          axis.text.x = element_text(size = x_text_size),
          axis.title.x = element_text(size = x_title_size)) +
    geom_text(data = summary_df, aes(y = study, x = max(full_alpha_trimmed$alpha_value, na.rm = TRUE),
                                     label = paste(sprintf("%.2f", mean_alpha),
                                                   paste0("[", sprintf("%.2f", lower_ci),
                                                          ",", " ", sprintf("%.2f", upper_ci), "]"))),
              hjust = 1, nudge_x = CI_offset,
              fontface = "bold", family = "Times New Roman", size = CI_size)
}



