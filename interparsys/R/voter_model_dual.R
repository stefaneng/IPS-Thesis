library(ggplot2)

vm_dual_sim <- function(n = 4) {
  k <- 2 * n + 1
  m <- round(runif(k))

  # Poisson thinning/super...
  # Add extra events * 2 for better viz
  event_n <- rpois(1, lambda = k * 2)
  event_time <- sort(rexp(event_n, rate = 1))
  # Randomly select which Poisson process the event came from
  event_i <- sample(1:k, size = event_n, replace = TRUE)
  arrow_end <- event_i + sample(c(-1, 1), event_n, replace = TRUE)
  # Flip arrows past k
  arrow_end[arrow_end > k] <- k - 1
  arrow_end[arrow_end == 0] <- 2

  arrow_df <- data.frame(event_time, event_i, arrow_end)

  res <- matrix(m, nrow = event_n + 1, ncol = k, byrow = TRUE)

  for(i in 1:event_n) {
   if(i > 1) res[i, ] <- res[i - 1,]
    res[i, arrow_end[i]] <- res[i,event_i[i]]
  }


  lines_df <- data.frame(y = numeric(), yend = numeric(), x = integer(), xend = integer(), color = integer())
  for(j in 1:k) {
   lines_df[j, ] <- c(y = 0, yend = event_time[1], x = j, xend = j, color = res[1, j])
  }

  for(i in 2:event_n) {
   tmp_lines <- data.frame(y = numeric(), yend = numeric(), x = integer(), xend = integer(), color = integer())
   for(j in 1:k) {
     tmp_lines[j, ] <- c(y = event_time[i - 1], yend = event_time[i], x = j, xend = j, color = res[i - 1, j])
   }
   lines_df <- rbind(lines_df, tmp_lines)
  }

  ggplot(arrow_df) +
   geom_segment(aes(x = event_i, xend = arrow_end, y = event_time, yend = event_time),
                arrow = arrow(length = unit(0.02, "npc")),
                lineend = "round", linejoin = "bevel") +
   geom_segment(data = lines_df, aes(x = x, xend = xend, y = y, yend = yend, color = as.factor(color)))
}

set.seed(133)
vm_dual_sim(n = 2)
