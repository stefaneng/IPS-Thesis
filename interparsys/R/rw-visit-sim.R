rw_expected_prob <- function(n, i, p) {
  odds <- (1 - p) / p
  if(p != 1/2) {
    res <- (odds^i - odds^(i - 1)) / (odds^i - 1)
    if(i != n) (1 - p) * res
    else res
  }
}

rw_expected_visits <- function(n, p) {
  if(p != 1/2) {
    sapply(1:n, function(i) 1/expected_prob(n, i, p))
  }
}

#' Random walk simulation
rw_sim <- function(n, p = 1/2, include_steps = FALSE) {
  steps <- 0
  state <- n
  visits <- rep(0, n)
  while(state != 0) {
    # if(state == k && is_even) {
    #   t <- rexp(1, rate = n * k)
    # } else if (state == k && !is_even) {
    #   t <- rexp(1, rate = n * k - k^2)
    # } else {
    #   t <- rexp(1, rate = 2 * state * (n - state))
    # }
    steps <- steps + 1

    visits[state] <- visits[state] + 1
    if(state == n) {
      state <- n - 1
    } else {
      if(runif(1) < p) state <- state + 1
      else state <- state - 1
    }
  }
  res <- list(visits = visits, steps = steps)
  if(include_steps) res
  else res$visits
}

# For the contact proces
rw_rates <- function(n, lambda = 1) {
  c(sapply(seq(1, n - 1), function(j) {
    j + (n - j) * lambda
    }), n)
}

# n <- 8
# lambda <- 100
# p <- lambda / (lambda + 1)
# res <- rates(n, lambda = lambda) / expected_visits(n, p = p)
# res * res[n]
#
# rowMeans(replicate(10, rw_sim(n, p = p)))
#
#
# x <- seq(0, 3, length.out = 1000)
# plot(x, dexp(x, rate = 10000), type = "l")
