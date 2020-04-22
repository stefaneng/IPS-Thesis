library(ggplot2)
library(actuar)
library(tidyr)
library(here)

#' Simulate one results from the contact process
#' Only torus implemented now
contact_1d <- function(row, lambda, torus=TRUE) {
  res <- list(
    t = 0,
    new_row = row,
    suppressed = FALSE
  )
  # Sum of all the rates
  rate <- sum(lambda * row + m)
  if(rate > 0) {
    res$t <- rexp(1, rate)

    # Get the indices of the ones
    ones <- which(as.logical(row))
    if(length(ones) == 0) {
      return(res)
    } else {
      # Sample uniformly from the infected sites
      if(length(ones) == 1) {
        s <- ones
      } else {
        # Sample from the ones
        s <- sample(ones, 1)
      }

      # Turn the 1 into a 0
      if (runif(1) < 1 / (1 + lambda)) {
        res$new_row[s] <- 0
      } else {
        # Place a one on either the left or the right neighbor
        ns <- s
        if (runif(1) < .5) {
          # Place 1 on left neighbor
          if (torus) {
            ns <- ifelse(s - 1 > 0, s - 1, length(row))
          }
        } else {
          # Place 1 on right neighbor
          if(torus) {
            ns <- ifelse(s + 1 < length(row), s + 1, 1)
          }
        }

        res$suppressed <- row[ns] == 1
        res$new_row[ns] <- 1
      }
    }
  }
  res
}

contact_1d(c(0,1,0), 2)

simulate_contact_1d <-  function(lambda = 2, space = 500, time = 400, torus=FALSE, init_config=c("one", "random", "all")) {
  # results <- matrix(0, nrow = time, ncol = space)

  if(init_config == "random") {
    r <- round(runif(n))
  } else if (init_config == "all") {
    r <- rep(1, 10)
  } else {
    r <- matrix(0, nrow = n, ncol = n)
    mid <- floor(n / 2)
    r[mid] <- 1
  }

  i <- 1
  total_time <- 0
  # Set first time point
  results[time, ] <- r
}

# Each 1 waits exp(1) time then becomes a 0
# Each 1 waits exp(lambda) time then places this 1 on to one of the neighbors with probability 1/2^d
#  The birth is suppressed if there is already a 1 there.

simulate_contact <- function(lambda = 2, n = 50, iter = 1e6, imgN = 4, torus=FALSE, init_config=c("one", "random", "all")) {
  if(init_config == "random") {
    m <- matrix(round(runif(n^2)),n,n)
  } else if (init_config == "all") {
    m <- matrix(1,n,n)
  } else {
    m <- matrix(0, nrow = n, ncol = n)
    mid <- floor(n / 2)
    m[mid, mid] <- 1
  }

  # Save old par settings
  par(mar=c(1, 1, 1, 1), mfrow=c(2,2))
  image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), main = "Initial")

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
      image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), main = paste0("time ", round(total_time)))
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

set.seed(27)
png(here("figures/contact_simulation_torus_25_below_crit.png"))
simulate_contact(lambda = 0.25, n = 25, imgN = 3, iter = 9e2, torus = TRUE, init_config = "all")
dev.off()
