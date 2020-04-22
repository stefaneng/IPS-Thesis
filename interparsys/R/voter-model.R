library(ggplot2)
library(actuar)
library(tidyr)
library(here)

# t <- seq(.00001, 5, length.out = 1000)
# # lambdas <- c(1, 2, 3, 4)
# pi3 <- c(1/3, 1/3, 1/3)
# # pi3 <- c(1, 0, 0)
#
# Q <- matrix(c(
#   -4/3, 1/3, 1/3,
#   1/3, -4/3, 1/3,
#   1/3, 1/3, -4/3
# ), byrow = TRUE, nrow = 3)
#
# df <- data.frame(t = t, ft = dphtype(t, pi3, Q))
#
#
# voter_density <- function(df, title) {
#  ggplot(df) +
#   geom_line(aes(x = t, y = ft)) +
#   labs(title = title) +
#   ylab("") +
#   xlab("time") +
#   theme_minimal(base_size = 18) +
#   theme(panel.spacing = unit(2, "lines"))
# }
#
# voter_density(df, "")


simulate_voter <- function(n = 50, times = c(1, 2, 5), torus=TRUE, include_config = FALSE) {
  m <- matrix(round(runif(n^2)),n,n)
  # Save old par settings
  par(mar=c(1, 1, 1, 1), mfrow=c(2,2))
  image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), main = "Initial")

  max_time <- max(times)
  times_uniq <- sort(times)
  times_uniq <- times_uniq[!duplicated(times_uniq)]

  # Sample the times from gamma (Erlang)
  total_time <- 0
  i <- 1
  total_ones <- sum(m)
  # Keep track of index in each parition
  while(total_time <= max_time && (total_ones > 0 || total_ones == n^2)) {
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

    if (length(times_uniq) >= 1 && floor(total_time) == times_uniq[1]) {
     image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), main = paste0("time ", round(total_time)))
     # Remove the image time
     times_uniq <- times_uniq[-1]
    }
    i <- i + 1
  }

  # Show the remaining plots
  for(ti in times_uniq) {
   image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), main = paste0("time ", ti))
  }

  res <- list(
   total_time = total_time
  )

  if (include_config) {
   res$config <- m
  }

  res
}

set.seed(262)
png(here("figures/voter_simulation_torus_50.png"))
simulate_voter(n = 50, times = c(5,30,300))
dev.off()


