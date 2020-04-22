sample_Q <- function(Q, i) {
 r <- Q[i, ]

 suppressWarnings({
  res <- unlist(lapply(r, rexp, n = 1))
 })

 min_state <- which.min(res)
 list(
  state = min_state,
  time = res[min_state]
 )
}

#
sim_mc_cont <- function(Q, init_state = 1, log_times = FALSE) {
 t <- 0
 state_log <- NULL
 wait_log <- NULL
 state <- init_state
 i <- 0

 # End when we hit absorbing state
 while(Q[state,state] != 0) {
  i <- i + 1

  if (log_times) {
   state_log <- c(state_log, state)
  }

  res <- sample_Q(Q, state)
  w <- res$time
  state <- res$state
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

Q1 <- function(lambda) {
 matrix(c(
  0, 0, 0,
  1, -(1 + lambda), lambda,
  0, 2, -2
 ),
 byrow=TRUE,
 nrow=3
 )
}

QS4 <- function(lambda) {
 matrix(c(
  -4, 4, 0, 0, 0, 0,
  2 * lambda, -(2 * lambda + 3), 2, 1, 0, 0,
  0, 2 * lambda, -(2 * lambda + 2), 0, 2, 0,
  0, 4 * lambda, 0, -(4 * lambda + 2), 2, 0,
  0, 0, 2 * lambda, 0, -(2 * lambda + 1), 1,
  0, 0, 0, 0, 0, 0
 ),
 byrow = TRUE,
 ncol = 6)
}


N <- 1000
sim_mc_cont(Q1(2), init_state = 3,log_times = TRUE)
res <- replicate(N, sim_mc_cont(Q1(2), init_state = 3))
# contact_wait3(lambda, log_times = TRUE)
hist(res)

# Cycle with 4
N <- 100
res <- replicate(N, sim_mc_cont(QS4(1), init_state = 1))
# contact_wait3(lambda, log_times = TRUE)
hist(res)

sim_mc_cont(QS4(1), init_state = 1, log_times = TRUE)
QS4(2)

# MUCH BETTER WAY TO SIMULATE!!!
# http://www.louisaslett.com/PhaseType/
library(actuar)
S <- QS4(25)[-6, -6]
pi <- c(1, 0, 0, 0, 0)
x <- rphtype(50, pi, S)
hist(x)
