library(ggplot2)

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
      # Wait at a rate 2 (since we have both (1,0) and (0,1))
      # Or is it rate 1??
      # w <- rexp(1, 2)
      w <- rexp(1,2)
      t <- t + w
      # Go to state 2
      state <- 2
    }
    else if (state == 2) {
      # w <- rexp(1, 1 + lambda)
      #
      # u <- runif(1)
      # t <- t + w
      # if(u < lambda / (1 + lambda)) {
      #   # Go back to start
      #   state <- 1
      # } else {
      #   state <- 3
      # }
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

#lambdas <- c(.0001, 1, 2, 3, 4, 5, 10)
lambdas <- seq(1, 150, length.out = 25)
# lambdas <- exp(seq(1, 5, length.out = 25))
res_sum <- matrix(0, ncol = 3, nrow = length(lambdas))
res <- NULL

#colnames(res) <- c("lambda", "mean", "variance")
for(i in seq_along(lambdas)) {
  l <- lambdas[i]
  r <- replicate(500, contact_wait2(l))
  ro <- cbind(l, r)

  res <- rbind(res, ro)
  res_sum[i, ] <- c(l, mean(r), var(r))

}

res_df <- data.frame(res)
res_sum_df <- data.frame(res_sum)
colnames(res_df) <- c("lambda", "time")
colnames(res_sum_df) <- c("lambda", "mean", "variance")

ggplot(res_df) +
  geom_point(aes(x = lambda, y = time), alpha = 0.05) +
  geom_abline(slope = 1/2, intercept = 3/2, color = "red", alpha = 0.5) +
  geom_point(data = res_sum_df, aes(x = lambda, y = mean), color = "red", size = 1)



rlm <- lm(data = res_df, time ~ lambda)
summary(rlm)

# Model the variance

#m_var <- lm(data = res_sum_df, variance ~ I(lambda^2) + lambda)
#print(coef(lm_var))

#res_sum_df$preds <- predict(lm_var)

# expected_variance <- function(lambda) {
#   vy <-1/4 + 1 / (1 + lambda)^2
#   y2 <- (3 + lambda)^2 / (4 * (1 + lambda)^2)
#   vx <- lambda * (1 + lambda)
#   x2 <- (1 + lambda)^2
#
# #   (1/4 + 1 / (1 + lambda)^2) * lambda^2 + ((3 + lambda) / (2 + 2 * lambda))^2 * lambda * (1 + lambda) +  lambda * (1 + lambda) * (1/4 + 1 / (1 + lambda)^2) + 1/4 + 1 / (1 + lambda)^2
#
#   x2 * vy + y2 * vx + vx * vy
# }

expected_var <- function(L) {
  # V2 <- (3 * L^2 + 4 * L + 1) / 4
  # VL <- (1 + 3 * L) / (1 + L)
  # VL + V2

  ((1 + L)^2 + 4 + (3 + L)^2 * (1 + L))/(4 * (1 + L))

  # (L^3 + 8 * L^2 + 16 * L + 14) / (4 * (1 + L))

  #
}

expected_var2 <- function(L) {
  (L^2 + 7 * L + 10) / 4 + 1/(1 + L)
}

res_sum_df$expected <- expected_var(lambdas)
res_sum_df$expected2 <- expected_var2(lambdas)
# View(res_sum_df)

ggplot(res_sum_df) +
  geom_point(aes(x = lambda, y = variance)) +
  geom_line(aes(x = lambda, y = expected), color = "red")

# -4.3176273   0.9604514   5.1000814
# 66.551868    1.014866    2.259514
# -110.5298074    0.8606275   15.7500845


## Computing variance
# vx <- function(lambda) {
#   1/4 + 1 / (1 + lambda)^2
# }

l <- seq(.0001, 1000, length.out = 100)
plot(l, expected_var(l), type = "l")
df <- data.frame(lambda = l, expected = expected_var(l))
lm(expected ~ I(lambda^2) + lambda + I(1/(lambda + 1)) + I(1/(lambda + 1)^2), data = df)

sim <- function(lambda, n = 1000) {

  res <-rep(0, n)
  for(i in seq_len(n)) {
    N <- rgeom(1, 1 / (1 + lambda))
    S2 <- rexp(N + 1, 2)
    SL <- rexp(N + 1, 1 + lambda)
    res[i] <-sum(c(S2, SL))
  }
  res
}

res_sum <- matrix(0, ncol = 3, nrow = length(lambdas))
for(i in seq_along(lambdas)) {
  l <- lambdas[i]
  r <- sim(l)

  res_sum[i, ] <- c(l, mean(r), var(r))
}

res_sum_df <- data.frame(res_sum)
colnames(res_sum_df) <- c("lambda", "mean", "variance")

res_sum_df$expected <- expected_var(lambdas)

ggplot(res_sum_df) +
  geom_point(aes(x = lambda, y = variance)) +
  geom_line(aes(x = lambda, y = expected))

rlm <- lm(data = res_sum_df, variance ~ I(lambda^2) + lambda + I(1/(lambda + 1)) + I(1/(lambda + 1)^2))
summary(rlm)

sim_var <- function(lambda, n = 1000) {
  N <- rgeom(n, 1 / (1 + lambda)) + 1
  SL <- rexp(n, 1 + lambda)
  S2 <- rexp(n, 2)

  var(N * (S2 + SL))
}

lambdas <- seq(0, 100, length.out = 25)
df_var <- data.frame(lambda = lambdas, sim = sapply(lambdas, sim_var), expected = expected_var(lambdas))

ggplot(df_var) +
  geom_point(aes(x = lambda, y = sim)) +
  geom_line(aes(x = lambda, y = expected))

var(N * (S2 + SL)) - (expected_vl(lambda) + expected_v2(lambda))

(var(N) * var(S2) + var(N) * mean(S2)^2 + var(S2) * mean(N)^2) - expected_variance(lambda)
#mean(S2 + SL)^2 - expected_v(lambda)


f <- function(l) {
  (2 + 2 * l) / (1 + 2 * l)
}
lambda <- 2
Q <- matrix(c(1, -1, -lambda / (1 + lambda), 1), nrow = 2, byrow = TRUE)
solve(Q)