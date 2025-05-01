#' Maximum Likelihood Estimation Plotter
#'
#' This function evaluates and plots the log-likelihood for a parameter over a grid.
#' It uses the provided likelihood function and data to identify the maximum.
#'
#' @param lfun A log-likelihood function taking (x, param).
#' @param x A numeric vector of observations.
#' @param param A numeric vector of parameter values to evaluate.
#' @param ... Additional arguments to pass to the plot.
#'
#' @return A list containing:
#' \describe{
#'   \item{i}{Index of the maximum likelihood estimate.}
#'   \item{parami}{Parameter value corresponding to the maximum likelihood.}
#'   \item{yi}{Maximum log-likelihood value.}
#'   \item{slope}{Slope approximations around the maximum (if computable).}
#' }
#'
#' @examples
#' loglik_binom <- function(y, p) dbinom(y, size = 20, prob = p, log = TRUE)
#' y <- c(3, 3, 4, 3, 4, 5, 5, 4)
#' pvals <- seq(0.01, 0.5, length = 100)
#' mymaxlik(loglik_binom, y, pvals)
#' @importFrom graphics points axis
#' @export
mymaxlik <- function(lfun, x, param, ...) {
  np <- length(param)
  z <- outer(x, param, lfun)
  y <- apply(z, 2, sum)
  plot(param, y, col = "blue", type = "l", lwd = 2, ...)
  i <- max(which(y == max(y)))
  abline(v = param[i], lwd = 2, col = "red")
  points(param[i], y[i], pch = 19, cex = 1.5, col = "black")
  axis(3, param[i], round(param[i], 2))
  if (i - 3 >= 1 && i + 2 <= np) {
    slope <- (y[(i - 2):(i + 2)] - y[(i - 3):(i + 1)]) /
      (param[(i - 2):(i + 2)] - param[(i - 3):(i + 1)])
  } else {
    slope <- NA
  }
  return(list(i = i, parami = param[i], yi = y[i], slope = slope))
}
