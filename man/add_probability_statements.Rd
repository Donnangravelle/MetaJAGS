% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_probability_statements.R
\name{add_probability_statements}
\alias{add_probability_statements}
\title{add_probability_statements}
\usage{
add_probability_statements(model_string, prob_conditions, predictor_vars)
}
\arguments{
\item{model_string}{Model string specifying the parameters to be estimated}

\item{prob_conditions}{Probabilities to be estimated for the parameters}

\item{predictor_vars}{Predictor variables specifying in the model formula}
}
\value{
Probability statements to be included in the model string
}
\description{
Responsible for adding probability statements to the JAGSmeta model
}
\examples{
\dontrun{
add_probability_statements(model_string, prob_conditions, predictor_vars)
}
}
