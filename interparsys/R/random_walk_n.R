random_walk <- function(n, log_times = FALSE) {
  t <- 0
  state_log <- NULL
  wait_log <- NULL
  state <- n
  t <- 0

  while(state != 0) {
   t <- t + 1

   if (log_times) {
    state_log <- c(state_log, state)
   }

   if (state == n) {
     state <- state - 1
   }
   else {
     if(runif(1) < .5) {
       state <- state + 1
     } else {
       state <- state - 1
     }
   }
  }

  if (log_times) {
   list(time = t, states = state_log)
  } else {
   t
  }
}

N <- 1000
n <- 100
r <- replicate(N, random_walk(n))
ggplot(data.frame(res = r)) +
 geom_histogram(aes(res))
