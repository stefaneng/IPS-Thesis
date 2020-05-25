harm <- function(n) {
  unlist(lapply(n, function(i) sum(1 / seq_len(i))))
}

euler_masch <- 0.5772156649

n <- seq(0, 50, by = 1)
plot(n , harm(n) - log(n), type = "l")


plot(2 * n, harm(2 * n) - harm(n), type = "l")
df <- data.frame(n = 2 * n, error = (harm(2 * n) - harm(n)) - log(2))
round(df[df$n == 30,]$error, 3)
