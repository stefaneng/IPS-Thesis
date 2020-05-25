library(animation)

plot_ising <- function(m, animate = FALSE, ...) {
 image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(-1, 0, 1), ...)
 if(animate) ani.record()
}

nbhd <- function(m, i, j, torus = TRUE) {
 max_x <- dim(m)[2]
 max_y <- dim(m)[1]

 if (torus) {
  # Each node always has 4 neighbors with periodic boundary conditions
  neighbors <- 4
  left_i <- ifelse(i - 1 == 0, max_x, i - 1)
  # Wrap around right side
  right_i <- ifelse(i + 1 > max_x, 1, i + 1)
  # Wrap bottom
  down_j <- ifelse(j + 1 > max_y, 1, j + 1)
  # Wrap top
  up_j <- ifelse(j - 1 == 0, max_y, j - 1)

  c(m[left_i, j], m[right_i, j], m[i, up_j], m[i, down_j])
 } else {
  # TODO: Implement if not torus
 }
}

# TODO: Should vectorize this
local_energy <- function(config, i, j, J = 1) {
  - J * config[i,j] * sum(nbhd(config, i, j))
}

h_energy <- function(config, J = 1) {
  #h <- function(i,j) local_energy(config, i, j, J)
  # outer(1:nrow(config), 1:nrow(config), h)
 res <- matrix(0, nrow = nrow(config), ncol = ncol(config))
  total <- 0
  for(i in 1:nrow(config)) {
    for(j in 1:ncol(config)) {
      res[i,j] <- local_energy(config, i, j, J)
      total <- total + local_energy(config, i, j, J)
    }
  }
  #total
  res
}

simulate_ising <- function(J = 1, times = c(1, 2, 5), N = 25, animate = FALSE, title = TRUE) {
  config <- matrix(sample(c(-1, 1), size = N * N, replace = TRUE), ncol = N)
  plot_ising(config, animate = animate, main = ifelse(title, "Initial", ""))

  max_time <- max(times)
  times_uniq <- sort(times)
  times_uniq <- times_uniq[!duplicated(times_uniq)]

  total_time <- 0
  # Keep track of index in each parition
  while(total_time <= max_time) {
    # Always wait exp(1) ?
    total_time <- total_time + rexp(1, N^2)

    # sample a random point on the grid
    s <- sample(1:N^2, 1)
    i <- ifelse(s %% N == 0, N, s %% N)
    j <- ceiling(s / N)

    # From http://bit-player.org/2019/glaubers-dynamics
    S <- sum(nbhd(config, i, j))
    # Change in energy
    dH <- 2 * S * config[i,j]
    # Flip if the energy is reduced
    if(dH < 0) {
      config[i,j] = - config[i,j]
    } else {
      # Flip if < exp(-dH * J)
      if (runif(1) < exp(-dH * J)) {
        config[i,j] = - config[i,j]
      }
    }

    if (length(times_uniq) >= 1 && total_time >= times_uniq[1]) {
     plot_ising(config, animate = animate, main = ifelse(title, paste0("time ", round(total_time, digit = 2)), ""))
     # Remove the image time
     times_uniq <- times_uniq[-1]
    }
  }
}

