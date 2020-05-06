library(ggplot2)

harm <- function(n) {
  sum(1/seq_len(n))
}

h <- function(n) {
  k <- floor(n / 2)
  sum(1/(n - seq(1,k-1)))
}

expected_time <- function(n) {
  is_even = n %% 2 == 0
  k <- floor(n / 2)

  res <- h(n) # (harm(n - 1) - harm(k + 1))
  if(is_even) {
    (n - 1) * (1/n  + res)
  } else {
    (n - 1) * (1/(n - k) + res)
  }
}

#' Expected number of visits on a finite random walk with 0 as absorbing state
#' p is probability of going from i to i + 1
#' 1 - p is probability of going from i to i - 1
rw_visits <- function(n, p = 1/2, include_time = FALSE) {
  is_even = n %% 2 == 0
  k = floor(n / 2)
  time <- 0
  state <- k
  visits <- rep(0, k)
  while(state != 0) {
    if(state == k && is_even) {
      t <- rexp(1, rate = n * k)
    } else if (state == k && !is_even) {
      t <- rexp(1, rate = n * k - k^2)
    } else {
      t <- rexp(1, rate = 2 * state * (n - state))
    }
    time <- time + t

    visits[state] <- visits[state] + 1
    if(state == k) {
      state <- k - 1
    } else {
      if(runif(1) < p) state <- state + 1
      else state <- state - 1
    }
  }
  res <- list(visits = visits,
              # Exponential scaling for the constant
              time = (n - 1) *  time)
  if(include_time) res
  else res$visits
}

# Testing that the expected time matches the simulation
# Also, expected_time(4) = 1.75 just like the phase-type distribution
unlist(lapply(4:10, expected_time))
unlist(lapply(4:10, function(n) {
  mean(replicate(10000, rw_visits(n, include_time = TRUE)$time))
}))

N <- 1e5
res_df <- data.frame(n = seq(4, N), t = sapply(seq(4,N), expected_time))
m <- lm(t ~ n, data = res_df)
round(m$coefficients, 2)
res_df$preds <- predict(m)
# Boring plot, just linear
#ggplot(res_df) +
#  geom_line(aes(x = n, y = t))
