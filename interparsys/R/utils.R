index_to_xy <- function(m, i) {
  rows <- dim(m)[1]
  cols <- dim(m)[2]
  list(
   x = ifelse(i %% rows == 0, rows, i %% rows),
   y = ceiling(i / rows)
  )
}

#' Compute the variance of the phase-type distributions
#' @examples
#' var_phtype(pi5, QL3(4 ,onlyTrans = TRUE))
var_phtype <- function(prob, rates) {
  mphtype(2, prob, rates) - mphtype(1, prob, rates)^2
}

hazard_phtype <- function(t, prob, rates) {
  dphtype(t, prob, rates) / (1 - pphtype(t, prob, rates))
}

harm <- function(n) {
  if(length(n) > 1) {
    sapply(n, harm)
  } else {
    sum(1/seq_len(n))
  }
}

h <- function(n) {
  if(length(n) > 1) {
    # TODO: Need to do this smarter if n is a vector
    # Can compute it smarter by computing first then adding/substracting
    sapply(n, h)
  } else {
    is_even = n %% 2 == 0
    k <- floor(n / 2)

    if(is_even) harm(n) - harm(n - k)
    else harm(n - 1) - harm(n - k - 1)
  }
}