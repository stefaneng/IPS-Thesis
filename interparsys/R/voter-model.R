library(ggplot2)
#library(actuar)
library(tidyr)
library(here)
library(animation)

#'
#' complete means that all nodes are connected
simulate_voter_1d <- function(n = 50, max_iter = 1e4, torus=TRUE, complete_graph = FALSE, init_config=c("random", "split")) {
  # Initial state
  if(length(init_config) != 1 || init_config == "random") {
    m <- round(runif(n))
  } else {
    m <- rep(0, n)
    # Set the first half to ones
    m[1:floor(n/2)] <- 1
  }

  # History of the voter model
  res <- matrix(0, ncol = n, nrow = max_iter)

  total_time <- 0
  i <- 1
  total_ones <- sum(m)
  # Keep track of index in each parition
  while(i <= max_iter && total_ones > 0 && total_ones < n) {
    # sample a random point
    s <- sample(1:n, 1)
    old_value <- m[s]
    wait_time <- rexp(1, rate = n)
    total_time <- total_time + wait_time

    # Get the neighbors opinions
    neighbor_ones <- 0
    if (complete_graph) {
      # Sample one of the other points
      s2 <- sample((1:n)[-s], 1)
      # Update the value based on neighbors value
      m[s] <- m[s2]
    } else {
      if (torus) {
        # Each node always has 2 neighbors with periodic boundary conditions
        neighbors <- 2
        left <- ifelse(s - 1 == 0, n, s - 1)
        # Wrap around right side
        right <- ifelse(s + 1 > n, 1, s + 1)

        # Count all the neighbors that have 1
        neighbor_ones <- m[left] + m[right]
      } else {
        neighbors <- 0

        if(s - 1 > 0) {
          neighbors <- neighbors + 1
          neighbor_ones <- neighbor_ones + m[s - 1, y]
        }
        if(s + 1 <= n) {
          neighbors <- neighbors + 1
          neighbor_ones <- neighbor_ones + m[s + 1, y]
        }
      }

      # Choose 1 or 0 based on neighbors
      p <- neighbor_ones / neighbors
      m[s] <- as.integer(runif(1) < p)
    }

    # Keep track of total number of ones
    total_ones <- total_ones + (m[s] - old_value)
    #assertthat::are_equal(total_ones, sum(m))

    # Save the state history
    res[i,] <- m
    i <- i + 1
  }

  list(
    # Remove the excess rows
    res = res[1:min(i - 1, max_iter),],
    time = total_time
  )
}

plot_1d <- function(n = 50, max_iter = 1e4, ...) {
  res <- simulate_voter_1d(n, max_iter, ...)
  image(t(res$res), col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), axes = FALSE, xlab = "Space", ylab = "Time")

  at <- seq(0, 1, length.out = 6)
  axis(1, at = at, labels = seq(from = 0, to = n, length.out = length(at)))
  axis(2, at = at, labels = round(seq(from = 0, to = res$time, length.out = length(at))))
}

simulate_voter <- function(n = 50, times = c(1, 2, 5), torus=TRUE, include_config = FALSE, title = TRUE, animate = FALSE) {
  m <- matrix(round(runif(n^2)),n,n)

  image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), main = ifelse(title, "Initial", ""))
  if(animate) ani.record()

  max_time <- max(times)
  times_uniq <- sort(times)
  times_uniq <- times_uniq[!duplicated(times_uniq)]

  # Sample the times from gamma (Erlang)
  total_time <- 0
  i <- 1
  total_ones <- sum(m)
  # Keep track of index in each parition
  while(total_time <= max_time && (total_ones > 0 || total_ones < n^2)) {
    # sample a random point on the grid
    s <- sample(1:n^2, 1)
    x <- ifelse(s %% n == 0, n, s %% n)
    y <- ceiling(s / n)

    wait_time <- rexp(1, rate = n^2)
    total_time <- total_time + wait_time

    # Get the neighbors opinions
    neighbor_ones <- 0
    if (torus) {
      # Each node always has 4 neighbors with periodic boundary conditions
      neighbors <- 4
      left_x <- ifelse(x - 1 == 0, n, x - 1)
      # Wrap around right side
      right_x <- ifelse(x + 1 > n, 1, x + 1)
      # Wrap bottom
      down_y <- ifelse(y + 1 > n, 1, y + 1)
      # Wrap top
      up_y <- ifelse(y - 1 == 0, n, y - 1)

      # Count all the neighbors that have 1
      neighbor_ones <- m[left_x, y] + m[right_x, y] + m[x, up_y] + m[x, down_y]
    } else {
      neighbors <- 0

      if(x - 1 > 0) {
       neighbors <- neighbors + 1
       neighbor_ones <- neighbor_ones + m[x - 1, y]
      }
      if(x + 1 <= n) {
       neighbors <- neighbors + 1
       neighbor_ones <- neighbor_ones + m[x + 1, y]
      }
      if(y - 1 > 0) {
       neighbors <- neighbors + 1
       neighbor_ones <- neighbor_ones + m[x, y - 1]
      }
      if(y + 1 <= n) {
       neighbors <- neighbors + 1
       neighbor_ones <- neighbor_ones + m[x, y + 1]
      }
    }

    # Choose 1 or 0 based on neighbors
    p <- neighbor_ones / neighbors
    m[x,y] <- as.integer(runif(1) < p)

    total_ones <- sum(m)

    if (length(times_uniq) >= 1 && total_time >= times_uniq[1]) {
     image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), main = ifelse(title, paste0("time ", round(total_time, digit = 0)), ""))
      if(animate) ani.record()
     # Remove the image time
     times_uniq <- times_uniq[-1]
    }
    i <- i + 1
  }

  # Show the remaining plots
  for(ti in times_uniq) {
    image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), main = ifelse(title, paste0("time ", ti), ""))
    if(animate) ani.record()
  }

  res <- list(
   total_time = total_time
  )

  if (include_config) {
   res$config <- m
  }

  res
}

# Complete graph with split config
set.seed(13)
png(here("figures/voter_simulation_1d_complete_split_100.png"))
plot_1d(n = 100, max_iter = 3e4, complete_graph = TRUE, init_config = "split")
dev.off()

set.seed(13)
png(here("figures/voter_simulation_1d_300.png"))
plot_1d(n = 300, max_iter = 3e4)
dev.off()

set.seed(262)
png(here("figures/voter_simulation_torus_50.png"))
par(mar=c(1, 1, 1, 1), mfrow=c(2,2))
simulate_voter(n = 50, times = c(5,30,300))
dev.off()

set.seed(262)
png(here("figures/voter_simulation_torus_100.png"))
par(mar=c(1, 1, 1, 1), mfrow=c(2,2))
simulate_voter(n = 100, times = c(25,250,750))
dev.off()


set.seed(27)
# Create an animation of the voter model
#ani.record(reset = TRUE)
#oopts <- ani.options(interval = 0.1)
img_dir <- here("gifs/voter_images/")
png(paste0(img_dir, "voter_sim%04d.png"))
par(mar=c(0, 0, 0, 0))
simulate_voter(n = 50, times = seq(0, 500, by = 1), title = FALSE, animate = TRUE)
# ani.replay()
#saveGIF(ani.replay(), movie.name = paste0(here("gifs/test.gif")), )
dev.off()
system(paste0("convert -delay 10 ", img_dir, "*.png ", here("gifs/"), "voter_sim.gif"))
file.remove(list.files(path = img_dir, pattern=".png", full.names = TRUE))



