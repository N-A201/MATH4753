#' Bootstrap Confidence Interval and Histogram
#'
#' Performs bootstrap resampling to compute confidence intervals for a statistic (mean, median, etc.)
#' and plots a histogram of the bootstrap distribution with CI and point estimate annotations.
#'
#' @param iter Number of bootstrap iterations. Default is 10000.
#' @param x A numeric vector of data.
#' @param fun The statistic to compute (e.g., "mean", "median", "var"). Can be a function or a character string.
#' @param alpha Significance level. Default is 0.05 for a 95% CI.
#' @param cx Text size for plot annotations.
#' @param ... Additional arguments passed to `hist()`.
#'
#' @return A list with the bootstrap CI, the statistic, the original data, and all bootstrap samples (`xstat`).
#' @examples
#' set.seed(1)
#' myboot2(x = rnorm(20), fun = "mean")
#' @export
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  if (is.character(fun)) fun <- match.fun(fun)
  n <- length(x)
  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nr = n, nc = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun)
  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2))
  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Histogram of Bootstrap sample statistics", "\n",
                            "alpha=", alpha, " iter=", iter, sep = ""),
               ...)
  mat <- matrix(x, nr = length(x), nc = 1, byrow = TRUE)
  pte <- apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "Black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)
  invisible(list(ci = ci, fun = fun, x = x, xstat = xstat))
}
