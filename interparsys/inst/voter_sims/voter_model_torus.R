library(gtools)
library(igraph)
library(dplyr)

permutations(2, 6, v = c(0,1), repeats.allowed = TRUE)

create_graph <- function(x) {
  res <- NULL
  n <- length(x)
  for(i in seq(2, n)) {
    if(x[i-1] == x[i]) {
      res <- c(res, i-1, i)
    }
  }
  if(x[n] == x[1]) {
    res <- c(res, n, 1)
  }

  g <- graph(res, n = n, directed = FALSE)
  V(g)$value <- x
  g
}

g <- create_graph(c(1,0,1,1,0,1,1))
clusters(g)

#' Find the clusters in 1d lattice
blah <- function(x) {
  k <- 0
  n <- length(x)
  prev <- 0
  for(i in seq(1, n-1)) {
    if(x[i] == 1 && prev == 0) {
      k <- k + 1
    }
    prev <- x[i]
  }

  if(x[n] == 1 && x[n - 1] == 0 && x[1] == 0) {
    k <- k + 1
  }
  k
}

g <- make_empty_graph(directed = FALSE) %>%
 add_vertices(1, x = 1) %>%
 add_vertices(2, x = 0) %>%
 add_vertices(3, x = 1) %>%
 add_vertices(4, x = 0) %>%
 add_edges(c(1,2, 2,3, 3,4, 4,1))

g <- graph(c(1,2, 2,3, 3,4, 4,1), directed = FALSE)
V(g)$value <- c(1,0,1,0)

plot(g)
clusters(g)

component_distribution(g)

clusters(c(1,1,1,0,1,1))
