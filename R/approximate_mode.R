#' approximate_mode
#'
#' Description of what the function does.
#'
#' @param x alpha values for plotting point estimate
#' @return mode of alpha distribution
#' @export
#' @examples
#' \dontrun{
#' approximate_mode(x)
#'}
#'
#'
approximate_mode <- function(x) {
d <- density(x)
highest_density_index <- which.max(d$y)
mode_approx <- d$x[highest_density_index]
return(mode_approx)
}
