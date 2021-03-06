library(ggplot2)
library(dplyr)

contact_wait <- function(lambda, n = 1000) {

  res <-rep(0, n)
  for(i in seq_len(n)) {
    N <- rgeom(1, 1 / (1 + lambda))
    S2 <- rexp(N + 1, 2)
    SL <- rexp(N + 1, 1 + lambda)
    res[i] <-sum(c(S2, SL))
  }
  res
}

# This is the slow but explicit discretization of the markov chain
# contact_wait2 <- function(lambda, log_times = FALSE) {
#   # 1 = (1,1), 2 = (1,0)/(0,1), 3 = (0,0)
#   t <- 0
#   state_log <- 1
#   wait_log <- 0
#   state <- 1
#   i <- 0
#
#   while(state != 3) {
#     i <- i + 1
#     if (state == 1) {
#       w <- rexp(1,2)
#       t <- t + w
#       # Go to state 2
#       state <- 2
#     }
#     else if (state == 2) {
#       w1 <- rexp(1, 1)
#       w_lambda <- rexp(1, lambda)
#       if (w1 < w_lambda) {
#         # Go to state 3, finished
#         t <- t + w1
#         w <- w1
#         state <- 3
#       } else {
#         t <- t + w_lambda
#         w <- w_lambda
#         # Go back to beginning
#         state <- 1
#       }
#     }
#
#     if (log_times) {
#       state_log <- c(state_log, state)
#       wait_log <- c(wait_log, w)
#     }
#   }
#
#   if (log_times) {
#     list(time = t, states = state_log, wait_times = wait_log)
#   } else {
#     t
#   }
# }

expected_mean <- function(L) {
  3/2 + L/2
}

expected_var <- function(L) {
  # Theoretic variance
  (L^3 + 8 * L^2 + 16 * L + 14) / (4 * (1 + L))
}

lambdas <- seq(1, 200, length.out = 25)
res_sum <- matrix(0, ncol = 3, nrow = length(lambdas))
res <- NULL

N <- 1000
for(i in seq_along(lambdas)) {
  l <- lambdas[i]
  r <- contact_wait(l, N)
  ro <- cbind(l, r, expected_mean(l), expected_var(l))

  res <- rbind(res, ro)
  res_sum[i, ] <- c(l, mean(r), var(r))

}

res_df <- data.frame(res)
res_sum_df <- data.frame(res_sum)
colnames(res_df) <- c("lambda", "time", "expected_mean", "expected_var")
colnames(res_sum_df) <- c("lambda", "mean", "variance")

res_df$std_time <- (res_df$time - res_df$expected_mean) / sqrt(res_df$expected_var)

ggplot(res_df) +
  geom_point(aes(x = lambda, y = time), alpha = 0.1) +
  geom_abline(slope = 1/2, intercept = 3/2, color = "red", alpha = 0.5) +
  geom_point(data = res_sum_df, aes(x = lambda, y = mean), color = "red", size = 1)

rlm <- lm(data = res_df, time ~ lambda)
summary(rlm)

# Model the variance

res_sum_df$expected <- expected_var(lambdas)

ggplot(res_sum_df) +
  geom_point(aes(x = lambda, y = variance)) +
  geom_line(aes(x = lambda, y = expected), color = "red")

## Limiting distribution

lambda <- 10000
N <- 2000
r <- contact_wait(lambda, N)
lim_res <- cbind(lambda, r, expected_mean(lambda), expected_var(lambda))

lim_res_df <- data.frame(lim_res)
colnames(lim_res_df) <- c("lambda", "time", "expected_mean", "expected_var")
lim_res_df$std_time <- (lim_res_df$time - lim_res_df$expected_mean) / sqrt(lim_res_df$expected_var)

ggplot(lim_res_df) +
  geom_histogram(aes(time), bins = 50)

mean(lim_res_df$time)

# Standardized time
ggplot(lim_res_df) +
  geom_histogram(aes(std_time))

exp_rnd <- data.frame(random = rexp(n = N, rate = 1))
ggplot(lim_res_df) +
  geom_histogram(aes(time * 2 / (1 + lambda)), alpha = 0.5, fill = "blue") +
  geom_histogram(data = exp_rnd, aes(random), fill = "red", alpha = 0.5)

ggplot(lim_res_df) +
  geom_histogram(aes(time))

