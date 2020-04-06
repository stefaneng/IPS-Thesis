library(ggplot2)
library(actuar)

t <- seq(0, 10, length.out = 1000)
lambdas <- c(1, 2, 3, 4)

f <- function(lambda, x) {
  (2 * exp(1/2 * (sqrt(lambda^2 + 6 * lambda + 1) - lambda - 3) * x))/sqrt(lambda^2 + 6 * lambda + 1) - (2 * exp(1/2 * (-sqrt(lambda^2 + 6 * lambda + 1) - lambda - 3) * x))/sqrt(lambda^2 + 6 * lambda + 1)
}

df <- data.frame(do.call(rbind, lapply(lambdas, function(l) cbind(λ = l, t = t, ft = f(l, t)))))

ggplot(df) +
  geom_line(aes(x = t, y = ft)) +
  facet_wrap(~ λ, nrow = 2, labeller=label_both) +
  labs(title = "Complete contact process 2 nodes") +
  ylab("") +
  xlab("time") +
  theme_minimal(base_size = 18)  +
 theme(panel.spacing = unit(2, "lines"))

ggsave(filename = "complete_2_contact_phase_densities.png", path = here::here("figures"), dpi = 320, units = "mm", width = 200)

# Check that it integrates to 1 (e.g is a density)
g <- function(x) {
  f(2, x)
}

integrate(g, lower = 0, upper = Inf)

#' Q matrix for complete contact process with 2 nodes
#' Projected to the number of ones in the process
#' onlyTrans=TRUE remove the absorbing states
QC2 <- function(lambda) {
 A = matrix(c(
  -2, 2, 0,
  lambda, -(1 + lambda), 1,
  0, 0, 0
 ),
 byrow=TRUE,
 nrow=3
 )

 if(onlyTrans) A[-ncol(A), -ncol(A)]
 else A
}

#' Q matrix for complete contact process with 3 nodes
#' Projected to the number of ones in the process
#' onlyTrans=TRUE remove the absorbing states
QC3 <- function(lambda, onlyTrans=FALSE) {
  # 3, 2, 1, 0
  A <- matrix(c(
    -3, 3, 0, 0,
    2 * lambda, -(2 + 2 *lambda), 2, 0,
    0, 2 * lambda, -(1 + 2 * lambda), 1,
    0, 0, 0, 0
  ), byrow=TRUE, ncol = 4)

  if(onlyTrans) A[-ncol(A), -ncol(A)]
  else A
}

pi <- c(1, 0, 0)

df <- data.frame(do.call(rbind, lapply(lambdas, function(l) cbind(λ = l, t = t, ft = dphtype(t, pi, QC3(l,onlyTrans = TRUE))))))

ggplot(df) +
 geom_line(aes(x = t, y = ft)) +
 facet_wrap(~ λ, nrow = 2, labeller=label_both) +
 labs(title = "Complete contact process 3 nodes") +
 ylab("") +
 xlab("time") +
 theme_minimal(base_size = 18) +
 theme(panel.spacing = unit(2, "lines"))

ggsave(filename = "complete_3_contact_phase_densities.png", path = here::here("figures"), dpi = 320, units = "mm", width = 200)

# Get the moments
# mphtype(1, pi, QC3(3,onlyTrans = TRUE))

#' Q matrix for cycle contact process with 4 nodes
#' onlyTrans=TRUE remove the absorbing states
QS4 <- function(lambda, onlyTrans=FALSE) {
 A <- matrix(c(
  -4, 4, 0, 0, 0, 0,
  2 * lambda, -(2 * lambda + 3), 2, 1, 0, 0,
  0, 2 * lambda, -(2 * lambda + 2), 0, 2, 0,
  0, 4 * lambda, 0, -(4 * lambda + 2), 2, 0,
  0, 0, 2 * lambda, 0, -(2 * lambda + 1), 1,
  0, 0, 0, 0, 0, 0
 ), byrow = TRUE, ncol = 6)

 if(onlyTrans) A[-ncol(A), -ncol(A)]
 else A
}

pi <- c(1, 0, 0, 0, 0)

df <- data.frame(do.call(rbind, lapply(lambdas, function(l) cbind(λ = l, t = t, ft = dphtype(t, pi, QS4(l,onlyTrans = TRUE))))))

ggplot(df) +
 geom_line(aes(x = t, y = ft)) +
 facet_wrap(~ λ, nrow = 2, labeller=label_both) +
 labs(title = "Cycle contact process 4 nodes") +
 ylab("") +
 xlab("time") +
 theme_minimal(base_size = 18) +
 theme(panel.spacing = unit(2, "lines"))

ggsave(filename = "cycle_4_contact_phase_densities.png", path = here::here("figures"), dpi = 320, units = "mm", width = 200)
