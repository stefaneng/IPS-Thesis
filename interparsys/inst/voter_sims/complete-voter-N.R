library(ggplot2)

expected_time <- function(n) {
 if(length(n) > 1) {
  sapply(n, expected_time)
 } else {
  is_even = n %% 2 == 0
  k <- floor(n / 2)

  res <- h(n) # (harm(n - 1) - harm(n - k))
  if(is_even) {
   (n - 1) * (harm(n) - harm(n - k))#(1/n  + res)
  } else {
   (n - 1) * (harm(n - 1) - harm(n - k - 1))#(1/(n - k) + res)
  }
 }
}

n <- 6
k <- floor(n / 2)
# assertthat::are_equal(harm(n - 1) - harm(n - k), h(n))
#even <- seq(4, 10, by = 2)
#odd <- seq(5, 11, by = 2)
n <- seq(1e5, 2e5)
h(n)


h(seq(4, 10000))

#' Expected number of visits on a finite random walk with 0 as absorbing state
#' p is probability of going from i to i + 1
#' 1 - p is probability of going from i to i - 1
voter_complete_n <- function(n, p = 1/2, include_time = FALSE) {
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
unlist(lapply(20:40, function(n) {
 mean(replicate(1000, voter_complete_n(n, include_time = TRUE)$time)) / (n - 1)
}))

N <- 1e4
res_df_evens <- data.frame(n = seq(4, N, by = 2), t = sapply(seq(4,N, by = 2), expected_time), even = TRUE)
res_df_odds <- data.frame(n = seq(5, N, by = 2), t = sapply(seq(5,N, by = 2), expected_time), even = FALSE)
res_df <- rbind(res_df_evens, res_df_odds)
m <- lm(t ~ (n - 1), data = res_df_odds)
round(m$coefficients, 2)
res_df$preds <- predict(m)

# Boring plot, just linear
ggplot(res_df) +
 geom_point(aes(x = n, y = t, color = even))

plot(seq(4, 10), expected_time(seq(4, 10)))
expected_time(7) - expected_time(6)
