# MetaJAGS

This package provides functions to easily implement custom hierarchical
Bayesian meta-regressions using RJAGS. These models are introduced and
described in detail in Zapparrata (2024). This package is currently in
early beta, and only three main functions. Additional functions and
features, as well as refinements will be added frequently. Please
download the most recent GitHub version to have the most up-to-date
version.

## Installation

Instructions on how to install the package from GitHub:

    # Install the development version from GitHub:
    devtools::install_github("Donnangravelle/MetaJAGS")

## JAGSmeta

JAGSmeta() is the main function provided by this package. It fits the
hierarchical Bayesian meta-regression, as described in Zapparrata
(2024).

At the moment, the function has 8 parameters.

formula: The model formula. This follows standard model formula syntax
in R. E.g., yi ~ 1 + predictor\_1 + predictor\_2

v: The column that contains the variances of the effect sizes to be
estimated.

studyID: The column that contains the unique study identifiers that
effects will be nested within.

data: The data frame object that contains the effect sizes, predictors,
variances, and study identifers. At the moment, the function should only
work with data frame objects, not with seperate vectors containing this
information.

n\_chains: The number of separate sampling chains that will be used to
simulate the distribution of effects.

n\_updates: The number of times the sampling model will update and
refit.

n\_iterations: The number of iterations used when sampling.

prob\_conditions: This allows users to estimate the probability of a
model parameter being of a certain value. Takes as an arugment a list of
model parameters and a list of values per parameter. Currently, there
are three options for estimation: “greater”: Probability that the model
parameter is greater than some value. “lesser”: Probability that the
model parameter is less than some value. “Between”: Probability that the
model parameter is between two values.

### Example


     result <- JAGSmeta(
     formula = yi ~ 1 + moderator_1 + moderator_2, # Model formula
     v = "vi", # Column name for variance
     studyID = "study_id", # Column name for study identifier
     data = example_data, # The dataset
     n_chains = 5, # Number of MCMC chains
     n_updates = 100000, # Number of updates for burn-in
     n_iterations = 100000 # Number of iterations for sampling
      prob_conditions = list(
     "mu.alpha" = list(
       "greater" =c(0,.2, .5, .8)),
     "sd.alpha" = list(
       "lesser" = .1,
       "between" = list(c(.1,.2), c(.2,.5)),
       "greater" = .5)
       )
    )

### Features in Development

These are not in any order.

1.  The ability to specify priors for each model parameter.

Model parameters are currently implemented by uniformative priors. Users
will be able to specfiy prior distributions, such as Cauchy,
half-Cauchy, and t-distributions to fully make use of the power of
Bayesian analyses.

1.  Improved model output.

At the moment, model parameters are outputted in hard to interpret
verbiage and layout. Model output will be adjusted to become easier to
understand.

1.  More flexible modeling options

Currently, the model is unable to handle interactions.

## JAGS.summary

This is a supplementary function for JAGSmeta. It simplifies the model
output to ignore parameters estimated at the study level. Takes as an
arugment the JAGSmeta model.

### Example

    JAGS.summary(JAGSmeta_model)

## BayesForest

This allows for the visualization of the distribution of effect size
estimates at the study level. The function uses the ggplot2 and ggridges
functions as a foundation. It takes several aesthetic arguments. The
main arguements are the model that was fitted, the original data, and
the column containing the study information to be displayed on the
y-axis.

### Example


    BayesForest(model, study_info = "Study_Info",
    data = example_data, dens_color = "skyblue",
    point_color = "black", overlap = 1,
    CI_offset = .3, y_text_size = 14, CI_size = 5, x_text_size = 14,
    x_title_size = 14, y_title_size = 14)

### Features in Development

At the moment, the function uses fixed sizes and positions for much of
the text. This means that users need to play with the sizes and
positions to work with their display. Future versions will make
positions and text sizes relative to the native R display by default.

## Functions and Features in the Pipeline

There are several features currently in development. Some examples of
include the ability to detect and correct for publication bias within a
Bayesian framework, additional options for visualizing the analysis, and
the ability to easily conduct a sensitivity analysis.
