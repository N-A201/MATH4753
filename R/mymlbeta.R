#' Maximum Likelihood Estimation for Beta Distribution
#'
#' This function estimates the shape parameters of a beta distribution via maximum likelihood,
#' using a grid-based approach and returns the MLEs with a 3D log-likelihood plot.
#'
#' @param x A numeric vector of values assumed to come from a Beta distribution.
#' @param a_vals A vector of possible values for shape1 (alpha).
#' @param b_vals A vector of possible values for shape2 (beta).
#' @param ... Additional graphical parameters passed to \code{persp()}.
#'
#' @return A list containing:
#' \describe{
#'   \item{shape1_hat}{The estimated MLE for shape1 (alpha).}
#'   \item{shape2_hat}{The estimated MLE for shape2 (beta).}
#'   \item{loglik}{The value of the log-likelihood at the maximum.}
#' }
#' @export
#' @examples
#' x <- rbeta(30, 3, 4)
#' a_vals <- seq(1, 6, length = 50)
#' b_vals <- seq(1, 6, length = 50)
#' result <- mymlbeta(x, a_vals, b_vals)
mymlbeta <- function(x, a_vals, b_vals, ...) {
  z <- outer(a_vals, b_vals, Vectorize(function(a, b) {
    if (a <= 0 || b <= 0) return(NA)
    sum(dbeta(x, a, b, log = TRUE))
  }))

  persp(a_vals, b_vals, z,
        theta = 40, phi = 30, expand = 0.6, col = "lightblue",
        xlab = "Shape1 (alpha)", ylab = "Shape2 (beta)", zlab = "Log-Likelihood",
        ticktype = "detailed", ...)

  m <- which(z == max(z, na.rm = TRUE), arr.ind = TRUE)
  list(shape1_hat = a_vals[m[1]], shape2_hat = b_vals[m[2]], loglik = z[m[1], m[2]])
}
