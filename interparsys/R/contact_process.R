library(ggplot2)
library(dplyr)

contact_wait2 <- function(lambda, log_times = FALSE) {
  # 1 = (1,1), 2 = (1,0)/(0,1), 3 = (0,0)
  t <- 0
  state_log <- 1
  wait_log <- 0
  state <- 1
  i <- 0

  while(state != 3) {
    i <- i + 1
    if (state == 1) {
      w <- rexp(1,2)
      t <- t + w
      # Go to state 2
      state <- 2
    }
    else if (state == 2) {
      w1 <- rexp(1, 1)
      w_lambda <- rexp(1, lambda)
      if (w1 < w_lambda) {
        # Go to state 3, finished
        t <- t + w1
        w <- w1
        state <- 3
      } else {
        t <- t + w_lambda
        w <- w_lambda
        # Go back to beginning
        state <- 1
      }
    }

    if (log_times) {
      state_log <- c(state_log, state)
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
  3/2 + L/2
}

expected_var <- function(L) {
  # Theoretic variance
  (L^3 + 8 * L^2 + 16 * L + 14) / (4 * (1 + L))
}

lambdas <- seq(1, 200, length.out = 25)
res_sum <- matrix(0, ncol = 3, nrow = length(lambdas))
res <- NULL

N <- 500
for(i in seq_along(lambdas)) {
  l <- lambdas[i]
  r <- replicate(N, contact_wait2(l))
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

l <- 10000
N <- 2000
r <- replicate(N, contact_wait2(l))
lim_res <- cbind(l, r, expected_mean(l), expected_var(l))

lim_res_df <- data.frame(lim_res)
colnames(lim_res_df) <- c("lambda", "time", "expected_mean", "expected_var")
lim_res_df$std_time <- (lim_res_df$time - lim_res_df$expected_mean) / sqrt(lim_res_df$expected_var)

# Standardized time
ggplot(lim_res_df) +
  geom_histogram(aes(std_time))

exp_rnd <- data.frame(random = rexp(n = N, rate = 2))
ggplot(lim_res_df) +
  geom_histogram(aes(time), alpha = 0.5, fill = "blue") +
  geom_histogram(data = exp_rnd, aes(random), fill = "red", alpha = 0.5)

ggplot(lim_res_df) +
  geom_histogram(aes(time))

mean(lim_res_df$std_time)

library(libstableR)

#  alpha, beta, sigma and mu estimates for stable distribution
pars_est_M <- stable_fit_init(lim_res_df$std_time)

pars_est_K <- stable_fit_koutrouvelis(lim_res_df$std_time, pars_est_M)
# Using maximum likelihood estimator, with McCulloch estimation
# as a starting point:
pars_est_ML <- stable_fit_mle(lim_res_df$std_time, pars_est_M)
# Using modified maximum likelihood estimator (See [1]):
pars_est_ML2 <- stable_fit_mle2d(lim_res_df$std_time, pars_est_M)

saveRDS(list(pars_est_M = pars_est_M, pars_est_K = pars_est_K, pars_est_ML = pars_est_ML, pars_est_ML2 = pars_est_ML2,
     N = N, l = l, lim_res_df = lim_res_df), file = paste0("stableEstimates", N, ".rds"))

# stableEst <- readRDS( paste0("stableEstimates", N, ".rds"))

rnd <- data.frame(random = stable_rnd(N, pars_est_M))
ggplot(lim_res_df) +
  geom_histogram(aes(std_time), alpha = 0.5, fill = "blue") +
  geom_histogram(data = rnd, aes(random), fill = "red", alpha = 0.5)
