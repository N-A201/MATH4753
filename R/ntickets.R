#' Determine Optimal Number of Tickets for Overbooking
#'
#' Calculates the number of tickets to sell given the number of seats (N),
#' the probability that a passenger shows up (p), and the allowable overbooking probability (gamma).
#' The function computes the optimal number of tickets using:
#' A discrete binomial approach,
#' A normal approximation (with continuity correction).
#' It also produces two separate plots: one for the discrete objective function and one for the continuous objective.
#'
#' @param N Integer. Number of seats on the flight.
#' @param gamma Numeric. Allowable probability of overbooking.
#' @param p Numeric. Probability that a passenger shows up.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{nd}{Optimal tickets using the discrete binomial method.}
#'   \item{nc}{Optimal tickets using the normal approximation.}
#'   \item{N}{The input number of seats.}
#'   \item{p}{The input probability of show.}
#'   \item{gamma}{The input overbooking probability.}
#' }
#'
#' @examples
#' \dontrun{
#' results <- ntickets(N = 400, gamma = 0.02, p = 0.95)
#' print(results)
#' }
#'
#' @export
ntickets <- function(N, gamma, p) {

  # Calculating optimal tickets for both
  nd <- NA
  nc <- NA
  for(n in N:(N+200)) {
    # Discrete check: P(X <= N) where X ~ Binomial(n, p)
    prob_not_overbooked <- pbinom(N, size = n, prob = p)
    if(prob_not_overbooked >= 1 - gamma && is.na(nd)) {
      nd <- n
    }

    # Normal approximation check with continuity correction.
    z <- (N + 0.5 - n*p) / sqrt(n*p*(1 - p))
    prob_not_overbooked_approx <- pnorm(z)
    if(prob_not_overbooked_approx >= 1 - gamma && is.na(nc)) {
      nc <- n
    }

    if(!is.na(nd) && !is.na(nc)) break
  }

  # Create vectors for plotting objective functions
  nvec <- seq(N, N + 50, by = 1)

  # Discrete objective function
  obj_discrete <- 1 - gamma - pbinom(N, size = nvec, prob = p)

  # Continuous (normal) objective function
  zvals <- (N + 0.5 - nvec * p) / sqrt(nvec * p * (1 - p))
  obj_continuous <- 1 - gamma - pnorm(zvals)

  # Discrete objective
  plot(
    nvec, obj_discrete, type = "l", col = "blue",
    xlab = "Number of Tickets (n)",
    ylab = "Objective Function Value",
    main = "Discrete Binomial Objective"
  )
  abline(h = 0, lty = 2)

  # Continuous objective
  plot(
    nvec, obj_continuous, type = "l", col = "red",
    xlab = "Number of Tickets (n)",
    ylab = "Objective Function Value",
    main = "Normal Approximation Objective"
  )
  abline(h = 0, lty = 2)

  # Return the results
  return(list(
    nd = nd,
    nc = nc,
    N = N,
    p = p,
    gamma = gamma
  ))
}
