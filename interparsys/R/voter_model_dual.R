library(ggplot2)
library(tidyr)

#' Creates a visualzation of the voter model dual
#' If pretty = TRUE, then does not actually use exponential distributions but attempt to "look" random
vm_dual_sim <- function(n = 4, pretty = FALSE) {
  n <- 4
  k <- 2 * n + 1
  m <- round(runif(k))

  x_indices <- c(-seq_len(n), 0, seq_len(n))
  # Poisson thinning/super...
  event_n <- rpois(1, lambda = k)
  event_time <- sort(rexp(event_n, rate = 1))
  # Randomly select which Poisson process the event came from
  event_i <- sample(1:k, size = event_n, replace = TRUE)
  arrow_end <- event_i + sample(c(-1, 1), event_n, replace = TRUE)
  # Flip arrows past k
  arrow_end[arrow_end > k] <- k - 1
  arrow_end[arrow_end == 0] <- 2

  arrow_df <- data.frame(event_time, event_i, arrow_end)

  # Keep track of the dual process A(t)
  # Probability just need the last row of it.
  dual_res <- matrix(nrow = event_n + 1, ncol = k, byrow = TRUE)
  dual_res[1, ] <- seq_len(k)

  res <- matrix(nrow = event_n + 1, ncol = k, byrow = TRUE)

  res[1,] <- m
  for(i in 1:(event_n + 1)) {
   if(i > 1) {
     res[i, ] <- res[i - 1,]
     dual_res[i, ] <- dual_res[i - 1,]
   }
    res[i, arrow_end[i]] <- res[i,event_i[i]]
    dual_res[i, arrow_end[i]] <- dual_res[i, event_i[i]]
  }

  dual_res_df <- data.frame(
    x = seq_len(k),
    dual = x_indices[dual_res[nrow(dual_res), ]]
  )

  res_df <-  data.frame(res)
  colnames(res_df) <- 1:ncol(res_df)
  res_df$event_time <- c(0, event_time)
  res_df$event_end <- c(event_time, max(event_time) + .1)
  res_long <- gather(res_df, x, value, `1`:`9`)
  res_long$x <- as.integer(res_long$x)

  voter_plot <- ggplot(res_long) +
   geom_rect(aes(xmin = x - .5, xmax = x + .5, ymin = event_time, ymax = event_end, fill = as.factor(value))) +
   theme_minimal() +
   scale_x_continuous("", breaks = seq_len(k), labels = x_indices) +
   theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
   )

  # Compute where each of the line segements should be draw and the color
  # Stupidly draws segements for each lattice value and exponential time
  # Could simplify this a bit but seems more complicated
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

  dual_plot <- ggplot(arrow_df) +
   # Plot the arrows
   geom_segment(aes(x = event_i, xend = arrow_end, y = event_time, yend = event_time),
                arrow = arrow(length = unit(0.02, "npc")),
                lineend = "round", linejoin = "bevel") +
   # Plot the line segements
   geom_segment(data = lines_df, aes(x = x, xend = xend, y = y, yend = yend, color = as.factor(color))) +
   geom_text(data = dual_res_df, aes(x = x, y = max(event_time) + .2, label = dual)) +
   # Show the dual process at finish time
   scale_x_continuous("", breaks = seq_len(k), labels = c(-seq_len(n), 0, seq_len(n))) +
   scale_y_continuous(expand = expansion(mult = c(0, .1))) +
   theme_minimal(base_size = 18) +
   theme_minimal() +
   theme(
    axis.line.x = element_line(colour = "grey", linetype = 2),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_blank()
    )

  #list(dual_plot = dual_plot, voter_plot = voter_plot)
  dual_plot
}

set.seed(134)
vm_dual_sim(n = 4)

