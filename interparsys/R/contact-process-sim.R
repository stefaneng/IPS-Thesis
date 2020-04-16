library(ggplot2)
library(actuar)
library(tidyr)
library(here)

# Sample: sample(1:5, 1, prob = c(.1, 0, .9, 0, 0))

# Each 1 waits exp(1) time then becomes a 0
# Each 1 waits exp(lambda) time then places this 1 on to one of the neighbors with probability 1/2^d
#  The birth is suppressed if there is already a 1 there.

simulate_contact <- function(lambda = 2, n = 50, iter = 1e6, imgN = 4, torus=FALSE, init_config=c("one", "random")) {
  if(init_config == "random") {
    m <- matrix(round(runif(n^2)),n,n)
  } else {
    # TODO: Need to fix this so that we have more that start as one
    m <- matrix(0, nrow = n, ncol = n)
    mid <- floor(n / 2)
    m[mid, mid] <- 1
  }

  # Save old par settings
  par(mar=c(1, 1, 1, 1), mfrow=c(2,2))
  image(m, axes=FALSE, col = grey(rev(seq(0, 1, length = 256))), main = "Initial")

  i <- 1
  total_time <- 0
  # Keep track of index in each parition
  part_i <- 1
  while(i <= iter) {
    # Each 1 waits exp(1) time then becomes a 0
    # Each 1 waits exp(lambda) time then places this 1 on to one of the neighbors with probability 1/2^d
    #  The birth is suppressed if there is already a 1 there.
    # sample a random point on the grid according to the rates
    rates <- sum(lambda * m + m)

    if(rates == 0) {
      break
    }
    # Wait exponential time according to sum of all the rates
    t <- rexp(1, rate = rates)
    total_time <- total_time + t
    # Sample from the ones according to the rates

    ones <- (1:n^2)[m == 1]
    if(length(ones) == 0) {
      break
    } else if(length(ones) == 1) {
      s <- ones
    } else {
      # Sample from the ones
      s <- sample(ones, 1)
    }

    x <- ifelse(s %% n == 0, n, s %% n)
    y <- ceiling(s / n)

    # Turn the 1 into a 0
    if (runif(1) < 1 / (1 + lambda)) {
      m[x,y] <- 0
    } else {
      # Randomly select one neighbor
      # 1 = bottom
      # 2 = left
      # 3 = top
      # 4 = right
      nx <- x
      ny <- y

      if (torus) {
        neighbor <- sample(1:4, 1)

        if(neighbor == 1) {
          ny <- ifelse(y + 1 > n, 1, y + 1)
        } else if (neighbor == 2) {
          nx <- ifelse(x - 1 == 0, n, x - 1)
        } else if (neighbor == 3) {
          ny <- ifelse(y - 1 == 0, n, y - 1)
        } else if (neighbor == 4) {
          nx <- ifelse(x + 1 > n, 1, x + 1)
        }
      } else {
        # available neighbors
        avail_neigh <- NULL

        # Botton
        if(y + 1 <= n) {
          avail_neigh <- c(1, avail_neigh)
        }
        # Left
        if(x - 1 > 0) {
          avail_neigh <- c(2, avail_neigh)
        }
        # Top
        if(y - 1 > 0) {
          avail_neigh <- c(3, avail_neigh)
        }
        # Right
        if(x + 1 <= n) {
          avail_neigh <- c(4, avail_neigh)
        }

        # Sample from the available neighbors
        neighbor <- sample(avail_neigh, 1)
        if(neighbor == 1) {
          ny <- y + 1
        } else if (neighbor == 2) {
          nx <- x - 1
        } else if (neighbor == 3) {
          ny <- y - 1
        } else if (neighbor == 4) {
          nx <- x + 1
        }
      }

      # Update the new neighbor to 1
      # Do we need to suppress it? Can't we just always place the 1 there?
      m[nx, ny] <- 1
    }

    if (i %% floor(iter / imgN) == 0) {
      image(m, axes=FALSE, col = grey(rev(seq(0, 1, length = 256))), main = paste0("time ", round(total_time)))
      part_i <- 0
    }
    i <- i + 1
    part_i <- part_i + 1
  }
}

set.seed(26)
png(here("figures/contact_simulation_torus_25.png"))
simulate_contact(lambda = 4, n = 25, imgN = 3, iter = 4e3, torus = TRUE, init_config = "one")
dev.off()

