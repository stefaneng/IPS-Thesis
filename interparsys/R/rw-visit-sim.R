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

expected_time(5)
unlist(lapply(4:10, expected_time))

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
    time <- time + (n - 1) *  t

    visits[state] <- visits[state] + 1
    if(state == k) {
      state <- k - 1
    } else {
      if(runif(1) < p) state <- state + 1
      else state <- state - 1
    }
  }
  res <- list(visits = visits, time = time)
  if(include_time) res
  else res$visits
}


cat(rw_visits(4, include_time = TRUE)$time, " expected: ", expected_time(4))

rw_visits(4, include_time = TRUE)

expected_time(4)

unlist(lapply(4:10, expected_time))
unlist(lapply(4:10, function(n) {
  mean(replicate(10000, rw_visits(n, include_time = TRUE)$time))
}))

N <- 100
n <- 10
k <- 10
#res <- rowMeans(replicate(N, {
#  res <- replicate(n, rw_visits(k, p = 2/3))
#  rowMeans(res)
#}))
#res


rw_q <- function(n, p = 1/2) {
  q <- matrix(0, nrow = n + 1, ncol = n + 1)
  q[1, 2] <- 1
  q[n+1,n+1] <- 1
  for(i in 2:n) {
    q[i, i + 1] <- p
    q[i, i - 1] <- 1 - p
  }

  q
}

rw_b <- function(n, p = 1/2) {
  diag(nrow = n) - rw_q(n, p)[-(n + 1), -(n + 1)]
}

n <- 10

solve(rw_b(4, 1/2))

unlist(lapply(n:1, function(i) {2*n*i - i^2}))

det(rw_b(3, 1/4))

#diag(nrow = k) + Reduce('+', lapply(1:4, function(i) rw_q(k)^i))
rw_q(k)
rw_q(k) %*% rw_q(k)
rw_q(k) %*% rw_q(k) %*% rw_q(k)
rw_q(k) %*% rw_q(k) %*% rw_q(k) %*% rw_q(k)
rw_q(k) %*% rw_q(k) %*% rw_q(k) %*% rw_q(k) %*% rw_q(k)

k <- 4
solve(diag(nrow = k) - rw_q(k)[-(k + 1), -(k + 1)])
k <- 5
solve(diag(nrow = k) - rw_q(k)[-(k + 1), -(k + 1)])
k <- 6
solve(diag(nrow = k) - rw_q(k)[-(k + 1), -(k + 1)]) %*% (diag(nrow = k) - rw_q(k)[-(k + 1), -(k + 1)])

# inverse {{1, -1, 0}, {-p, 1, -(1 - p)}, {0, -p, 1}}
# inverse {{1, -1, 0, 0}, {-p, 1, -(1 - p), 0}, {0, -p, 1, -(1 - p)}, {0, 0, -p, -1}}
