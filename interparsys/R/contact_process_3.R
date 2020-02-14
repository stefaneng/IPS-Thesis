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
      t <- t + w
      # Go to state 2
      state <- 2
    }
    else if (state == 2) {
      w1 <- rexp(1, 2)
      w_lambda <- rexp(1, 2 * lambda)

      if (w_lambda < w1) {
        # Go back to state 3
        t <- t + w1
        w <- w1
        state <- 3
      } else {
        # Go to state 2
        t <- t + w_lambda
        w <- w_lambda
        state <- 1
      }
    }
    else if (state == 1) {
      w1 <- rexp(1, 1)
      w_lambda <- rexp(1, 2 * lambda)

      if (w_lambda < w1) {
        # Go back to state 2
        t <- t + w1
        w <- w1
        state <- 2
      } else {
        # Go to state 1
        t <- t + w_lambda
        w <- w_lambda
        state <- 0
      }
    }

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

lambdas <- seq(1, 25, length.out = 10)
res_sum <- matrix(0, ncol = 3, nrow = length(lambdas))
res <- NULL

N <- 500
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
rpass <- replicate(N, contact_wait3(l, log_times = TRUE)$states)
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


# Create the Q matrix for the fundamental matrix of absorbing Markov chain
#
genQ3 <- function(lambda) {
  matrix(c(0, 2 * lambda / (1 + 2 * lambda), 0,
    2 / (2 + 2 * lambda), 0, 2*lambda / (2 + 2 * lambda),
    0, 1, 0), nrow = 3, byrow = TRUE)
}

lambdas <- seq(1, 100, by = 1)

# Compute the expected number of visits to each node
# Using the fundamental matrix inversion: (I - Q)^{-1}
dist_df <- data.frame(do.call(rbind, lapply(lambdas, function(x) solve(diag(3) - genQ3(x))[3,])))
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
