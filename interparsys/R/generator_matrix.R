#' Q matrix for complete contact process with 2 nodes
#' Projected to the number of ones in the process
#' onlyTrans=TRUE remove the absorbing states
QK2 <- function(lambda, onlyTrans = TRUE) {
  A <- matrix(c(
    -2, 2, 0,
    lambda, -(1 + lambda), 1,
    0, 0, 0
  ),
  byrow=TRUE,
  nrow=3
  )

  if(onlyTrans) A[-ncol(A), -ncol(A)]
  else A
}

#' Q matrix for complete contact process with 3 nodes
#' Projected to the number of ones in the process
#' onlyTrans=TRUE remove the absorbing states
QK3 <- function(lambda, onlyTrans=TRUE) {
  # 3, 2, 1, 0
  A <- matrix(c(
    -3, 3, 0, 0,
    2 * lambda, -(2 + 2 *lambda), 2, 0,
    0, 2 * lambda, -(1 + 2 * lambda), 1,
    0, 0, 0, 0
  ), byrow=TRUE, ncol = 4)

  if(onlyTrans) A[-ncol(A), -ncol(A)]
  else A
}

#' Contact process S4
#' Q matrix for cycle contact process with 4 nodes
#' onlyTrans=TRUE remove the absorbing states
QS4 <- function(lambda, onlyTrans=TRUE) {
  A <- matrix(c(
    -4, 4, 0, 0, 0, 0,
    2 * lambda, -(2 * lambda + 3), 2, 1, 0, 0,
    0, 2 * lambda, -(2 * lambda + 2), 0, 2, 0,
    0, 4 * lambda, 0, -(4 * lambda + 2), 2, 0,
    0, 0, 2 * lambda, 0, -(2 * lambda + 1), 1,
    0, 0, 0, 0, 0, 0
  ), byrow = TRUE, ncol = 6)

  if(onlyTrans) A[-ncol(A), -ncol(A)]
  else A
}

#' Contact process L3
QL3 <- function(lambda, onlyTrans=TRUE) {
  A <- matrix(c(
    -3, 2, 1, 0, 0, 0,
    lambda, -(lambda + 2), 0, 1, 1, 0,
    lambda, 0, -(lambda + 1), 1, 0, 0,
    0, lambda, 0, -(lambda + 1), 0, 1,
    0, 2 * lambda, 0, 0, -(2 * lambda + 1), 1,
    0, 0, 0, 0, 0, 0
  ), byrow = TRUE, ncol = 6)

  if(onlyTrans) A[-ncol(A), -ncol(A)]
  else A
}

QC4_voter <- function(onlyTrans = TRUE) {
  A <- matrix(c(
    -8/3, 8/3, 0,
    1, -2, 1,
    0, 0, 0
  ),
  byrow=TRUE,
  nrow=3
  )

  if(onlyTrans) A[-ncol(A), -ncol(A)]
  else A
}

QS4_voter <- function(onlyTrans = TRUE) {
  A <- matrix(c(
    -2, 1, 0, 1,
    2, -2, 0, 0,
    4, 0, -4, 0,
    0, 0, 0, 0
  ),
  byrow=TRUE,
  nrow=4
  )

  if(onlyTrans) A[-ncol(A), -ncol(A)]
  else A
}