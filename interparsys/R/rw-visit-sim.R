#' Expected number of visits on a finite random walk with 0 as absorbing state
#' p is probability of going from i to i + 1
#' 1 - p is probability of going from i to i - 1
rw_visits <- function(n, p = 1/2) {
  state <- n
  visits <- rep(0, n)
  while(state != 0) {
    visits[state] <- visits[state] + 1
    if(state == n) {
      state <- n - 1
    } else {
      if(runif(1) < p) state <- state + 1
      else state <- state - 1
    }
  }
  visits
}

rw_visits(10)

N <- 100
n <- 10
k <- 10
res <- rowMeans(replicate(N, {
  res <- replicate(n, rw_visits(k, p = 2/3))
  rowMeans(res)
}))
res


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

solve(rw_b(2, 1/4))
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

