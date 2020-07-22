library(ggplot2)
library(actuar)
library(tidyr)
library(here)


#' Simulates the 2D contact process and creates image plots at the given time points.
#' The behavior is as follows:
#' Each 1 waits exp(1) time then becomes a 0
#' Each 1 waits exp(lambda) time then places this 1 on to one of the neighbors with probability 1/2^d
#' The birth is suppressed if there is already a 1 there.
#' @param lambda
#' @param n the dimension for the n x n matrix of the contact process
#' @param times the times that the images should be displayed at
#' @param torus boolean representing if the edges should be wrapped around. If T then will wrap around.
#' @param init_config controls the initial 1 and 0 placement. "one" places one 1 at the origin. "random" assigns each cell randomly to 1 or 0. "all" assigns all cells to 1.
#' @param title adds the time in the title of the plots
#' @examples
#' \dontrun{
#' par(mar=c(1, 1, 1, 1), mfrow=c(2,2))
#' simulate_contact(lambda = 4, n = 50, torus = TRUE, times = c(5, 10, 30), init_config = "one")
#' }
simulate_contact <- function(lambda = 2, n = 50, times = c(1, 2, 5), torus=TRUE, init_config=c("one", "random", "all"), include_config = FALSE, title = TRUE) {
  if(init_config == "random") {
   m <- matrix(round(runif(n^2)),n,n)
  } else if (init_config == "all") {
   m <- matrix(1,n,n)
  } else {
   m <- matrix(0, nrow = n, ncol = n)
   mid <- floor(n / 2)
   m[mid, mid] <- 1
  }

  max_time <- max(times)
  times_uniq <- sort(times)
  times_uniq <- times_uniq[!duplicated(times_uniq)]

  image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), main = ifelse(title, "Initial", ""))

  i <- 1
  total_time <- 0
  # Keep track of index in each parition
  while(total_time <= max_time) {
    # Each 1 waits exp(1) time then becomes a 0
    # Each 1 waits exp(lambda) time then places this 1 on to one of the neighbors with probability 1/2^d
    #  The birth is suppressed if there is already a 1 there.
    # sample a random point on the grid according to the rates
    rates <- sum(lambda * m + m)

   if(rates == 0) {
     # Show the remaining plots
     for(ti in times_uniq) {
       image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), main = ifelse(title, paste0("time ", ti), ""))
     }
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

  if (length(times_uniq) >= 1 && total_time >= times_uniq[1]) {
    image(m, axes=FALSE, col = c("#FFFFFF", "#000000"), breaks = c(0, 1/2, 1), main = ifelse(title, paste0("time ", round(total_time)), ""))
    # Remove the image time
    times_uniq <- times_uniq[-1]
  }
  i <- i + 1
  }

  res <- list(
    total_time = total_time,
    survived = sum(m) > 0
  )
  if (include_config) {
    res$config <- m
  }

  res
}
