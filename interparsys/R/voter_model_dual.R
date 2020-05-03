library(ggplot2)
library(tidyr)

#' Creates a visualzation of the voter model dual
#' If pretty = TRUE, then does not actually use exponential distributions but attempt to "look" random
vm_dual_sim <- function(n = 4, pretty = FALSE, delta = .1) {
  k <- 2 * n + 1
  m <- round(runif(k))

  x_indices <- c(-n:-1, 0, seq_len(n))
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
  dual_res[1, ] <- seq_len(k)
  for(i in 1:(event_n + 1)) {
   if(i > 1) {
     res[i, ] <- res[i - 1,]
     dual_res[i, ] <- dual_res[i - 1,]
   }
    res[i, arrow_end[i]] <- res[i,event_i[i]]
    dual_res[i, arrow_end[i]] <- dual_res[i, event_i[i]]
  }

  # Calculate the most switches for the dual process A(t)
  # Will highlight this path to explain process
  max_switches <- which.max(apply(dual_res, 2, function(x) length(unique(x))))

  dual_example_df <- data.frame(x = integer(), xend = integer(), y = numeric(), yend = numeric())
  j <- 1
  x <- max_switches
  xend <- max_switches
  y <- max(event_time) + delta
  for(i in nrow(arrow_df):1) {
    if(arrow_df[i, "arrow_end"] == xend) {
      yend <- arrow_df[i, "event_time"]
      dual_example_df[j, ] <- c(x = x, xend = xend, y = y, yend = yend)
      j <- j + 1
      xend <- arrow_df[i, "event_i"]
      y <- yend
      dual_example_df[j, ] <- c(x = x, xend = xend, y = y, yend = yend)
      x <- xend
      j <- j + 1
    }
  }
  dual_example_df[j, ] <- c(x = xend, xend = xend, y = yend, yend = 0)

  dual_df <- data.frame(
    x = seq_len(k),
    dual = x_indices[dual_res[nrow(dual_res), ]]
  )

  res_df <-  data.frame(res)
  colnames(res_df) <- 1:ncol(res_df)
  res_df$event_time <- c(0, event_time)
  res_df$event_end <- c(event_time, max(event_time) + delta)
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

  for(j in 1:k) {
   tmp_lines[j, ] <- c(y = event_time[event_n], yend = event_time[event_n] + delta, x = j, xend = j, color = res[event_n, j])
  }

  lines_df <- rbind(lines_df, tmp_lines)

  dual_plot <- ggplot(arrow_df) +
   # Plot the arrows
   geom_segment(aes(x = event_i, xend = arrow_end, y = event_time, yend = event_time),
                arrow = arrow(length = unit(0.02, "npc")),
                lineend = "round", linejoin = "bevel") +
   # Plot the line segements
   geom_segment(data = lines_df, aes(x = x, xend = xend, y = y, yend = yend, color = as.factor(color))) +
   geom_text(data = dual_df, aes(x = x, y = max(event_time) + .2, label = dual)) +
   # Show the dual process at finish time
   scale_x_continuous("", breaks = seq_len(k), labels = x_indices) +
   scale_y_continuous(expand = expansion(mult = c(0, delta))) +
   theme_minimal(base_size = 18) +
   theme_minimal() +
   theme(
    axis.line.x = element_line(colour = "grey", linetype = 2),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_blank()
    )

  example_path_plot <-
   ggplot(arrow_df) +
   # Plot the arrows
   geom_segment(aes(x = event_i, xend = arrow_end, y = event_time, yend = event_time),
                arrow = arrow(length = unit(0.02, "npc")),
                lineend = "round", linejoin = "bevel", alpha = .3) +
   # Plot the line segements
   geom_segment(data = lines_df,
                aes(x = x, xend = xend, y = y, yend = yend, color = as.factor(color)),
                alpha = .3) +
   # Plot the example line segement
   # This line has the maximum number of changes
   geom_segment(data = dual_example_df,
                aes(x = x, xend = xend, y = y, yend = yend)) +
   # Show the dual process at finish time
   geom_text(data = dual_df, aes(x = x, y = max(event_time) + 2 * delta, label = dual)) +
   # Annotate the dual process
   annotate("text", x =  k + 1, y = max(event_time) + 2 * delta, label = "A(t)") +
   scale_x_continuous("", breaks = seq_len(k), labels = x_indices) +
   scale_y_continuous(expand = expansion(mult = c(0, delta))) +
   coord_cartesian(xlim = c(1, k),clip = "off") +
   theme_minimal(base_size = 18) +
   theme_minimal() +
   theme(
    axis.line.x = element_line(colour = "grey", linetype = 2),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_blank()
   )

  list(dual_plot = dual_plot, voter_plot = voter_plot, example_path_plot = example_path_plot)
}

set.seed(135)
v <- vm_dual_sim(n = 5)
v$example_path_plot
v$dual_plot

