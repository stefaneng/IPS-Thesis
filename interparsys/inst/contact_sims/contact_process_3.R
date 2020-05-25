library(tidyr)
library(dplyr)
library(ggplot2)

# This is the slow but explicit discretization of the markov chain
contact_wait3 <- function(lambda, log_times = FALSE) {
  t <- 0
  state_log <- NULL
  wait_log <- NULL
  state <- 3
  i <- 0

  while(state != 0) {
    i <- i + 1

    if (log_times) {
      state_log <- c(state_log, state)
    }

    if (state == 3) {
      w <- rexp(1,3)
      # Go to state 2
      state <- 2
    }
    else if (state == 2) {
      w1 <- rexp(1, 2)
      w_lambda <- rexp(1, 2 * lambda)

      if (w_lambda < w1) {
        # Go back to state 3
        w <- w_lambda
        state <- 3
      } else {
        # Go to state 2
        w <- w1
        state <- 1
      }
    }
    else if (state == 1) {
      w1 <- rexp(1, 1)
      w_lambda <- rexp(1, 2 * lambda)

      if (w_lambda < w1) {
        # Go back to state 2
        w <- w_lambda
        state <- 2
      } else {
        # Go to state 1
        w <- w1
        state <- 0
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

expected_mean <- function(L) {
  #(2 * L^2 + L + 1) / 3 + (2 * L + 1) / 2 + 1
  (4 * L^2 + 8 * L + 11) / 6
}

# Create the Q matrix for the fundamental matrix of absorbing Markov chain
#
genQ3 <- function(lambda) {
  matrix(c(0, 2 * lambda / (1 + 2 * lambda), 0,
           2 / (2 + 2 * lambda), 0, 2*lambda / (2 + 2 * lambda),
           0, 1, 0), nrow = 3, byrow = TRUE)
}

# testVisitCount <- function(L) {
#   c(2 * L + 1, (L + 1) * (2 * L + 1) , 2 *L^2 + L + 1)
# }

# Number of times expected to visit each state
visitCount <- function(lambda) {
  solve(diag(3) - genQ3(lambda))[3,]
}

# This one seems correct
# The expected time spent in each state
expectedTime <- function(lambda) {
  sum(visitCount(lambda) * c(1/(1 + 2 * lambda), 1/(2 + 2 * lambda), 1/3))
}

expectedTime(2)

# Simulate lambda = 2
N <- 1000
lambda <- 2
res <- replicate(N, contact_wait3(lambda))
# contact_wait3(lambda, log_times = TRUE)
c(sim = mean(res), expected = expectedTime(lambda))

# Show that it equals in distribution exponential
hist(res * 3 / (2 * lambda^2 + lambda + 1), freq = FALSE)
xvals <- seq(0, 50, by=0.5)
lines(xvals,
      dexp(xvals, rate = 1),
      lwd=2,
      col="red")


# Simulate more lambdas
lambdas <- seq(1, 25, length.out = 10)
res_sum <- matrix(0, ncol = 3, nrow = length(lambdas))
res <- NULL

for(i in seq_along(lambdas)) {
  l <- lambdas[i]
  r <- replicate(N, contact_wait3(l))
  ro <- cbind(l, r)

  res <- rbind(res, ro)
  res_sum[i, ] <- c(l, mean(r), var(r))
}

res_df <- data.frame(res)
res_sum_df <- data.frame(res_sum)
colnames(res_df) <- c("lambda", "res")
colnames(res_sum_df) <- c("lambda", "mean", "variance")
ggplot(res_sum_df) +
  geom_line(aes(lambda, mean))

# Plot the time distribution
lambda <- 5
r <- replicate(N, contact_wait3(lambda))
ggplot(data.frame(res = r)) +
  geom_histogram(aes(res))

lambda <- 2
rpass <- replicate(N, contact_wait3(lambda, log_times = TRUE)$states)
# Finding distribution of the number of times at each state
pass_df <- as.data.frame(do.call(rbind, lapply(rpass, table))) %>% gather(state, count)

# Estimate the parameters for geometric distribution
params <- pass_df %>%
  group_by(state) %>%
  summarize(est = N / sum(count))

# Computed from the fundamental matrix
params$expected <- 1 / c(1 + 2 * lambda, 1 + 3 * lambda + 2 * lambda^2, 1 + lambda + 2 * lambda^2)

# Sample from geometric distributions
# Supports the claim that the number of times in each node is geometrically distributed
# But why?
pass_df$expected <- unlist(lapply(params$expected, function(p) rgeom(N, p) + 1))
ggplot(pass_df) +
  geom_histogram(aes(count), fill = "blue", alpha = 0.5) +
  geom_histogram(aes(expected), fill = "red", alpha = 0.5) +
  facet_wrap(~ state, scales = "free")

ggplot(pass_df) +
  geom_histogram(aes(count, fill = state))

r <- replicate(1000, contact_wait3(2))
ggplot(data.frame(value = r)) +
  geom_histogram(aes(value), bins = 30)

lambdas <- seq(1, 100, by = 1)

# Compute the expected number of visits to each node
# Using the fundamental matrix inversion: (I - Q)^{-1}
# Wolfram alpha: https://www.wolframalpha.com/input/?i=inverse+of+%7B%7B1%2C+-2x+%2F+%281+%2B+2x%29%2C+0%7D%2C+%7B-2%2F%282+%2B+2x%29%2C+1%2C+-+2x+%2F+%282+%2B+2x%29%7D%2C+%7B0%2C+-1%2C+1%7D%7D
dist_df <- data.frame(do.call(rbind, lapply(lambdas, visitCount)))
colnames(dist_df) <- c("state1", "state2", "state3")
dist_df$lambda <- lambdas

lm(state1 ~ lambda, data = dist_df)
lm(state2 ~ I(lambda^2) + lambda, data = dist_df)
lm(state3 ~ I(lambda^2) + lambda, data = dist_df)

dist_long_df <- dist_df %>% gather(state, expected, -lambda)

ggplot(dist_long_df) +
  geom_line(aes(x = lambda, y = expected)) +
  facet_wrap(~ state, scales = "free")


res2 <- replicate(1000, {
  res <- contact_wait3(2, log_times = TRUE)
  sum(res$wait_times[res$states == 3])
})

mean(res2)
