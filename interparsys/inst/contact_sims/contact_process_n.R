library(tidyr)
library(dplyr)
library(ggplot2)

# This is the slow but explicit discretization of the markov chain
contact_waitn <- function(lambda, n = 5, log_times = FALSE) {
  t <- 0
  state_log <- NULL
  wait_log <- NULL
  state <- n
  i <- 0

  while(state != 0) {
    i <- i + 1

    if (log_times) {
      state_log <- c(state_log, state)
    }

    if (state == n) {
      w <- rexp(1, n)
      # Go to state 2
      state <- n - 1
    }
    else {
      w1 <- rexp(1, state * (n - state) * lambda)
      w2 <- rexp(1, state)
      if (w1 < w2) {
        w <- w1
        state <- state + 1
      } else {
        w <- w2
        state <- state - 1
      }
    }

    t <- t + w
    if (log_times) {
      wait_log <- c(wait_log, w)
    }
  }

  if (log_times) {
    list(time = t, states = state_log, wait_times = wait_log)
  } else {
    t
  }
}

lambda <- 2
contact_waitn(2, n = 3)
res <- replicate(N, contact_waitn(2, n = 10))
# contact_wait3(lambda, log_times = TRUE)
hist(res)


{
  {1, - 2x / (2x + 1), 0, 0, 0},
  {0, 1 - x/(1 + x), 0, 0, 0},
  {-x/(x + 1), 0, 1, -1/(x + 1), -1/(x + 1)},
  {-x/(x + 2), 0, 0, 1 -2 / (x + 2), 0},
  {0, -2/3, -1/3, 0, 1}
}



{{1, -1, 0},{-x/(2 + x), 1, -2/(2 + x)},{0, -2x/(1 + 2x), 1}}

{{0, 1, 0},{x/(2 + x), 0, 2/(2 + x)},{0, 2x/(1 + 2x), 0}}
