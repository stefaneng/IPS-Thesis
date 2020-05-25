library(interparsys)
library(ggplot2)
#library(gridExtra)
library(ggpubr)
library(actuar)
library(tidyr)

t <- seq(0, 10, length.out = 1000)
lambdas <- c(0, 1, 2, 3, 4)
pi2 <- c(1, 0)
pi3 <- c(1, 0, 0)
pi5 <- c(1, 0, 0, 0, 0)

f <- function(lambda, x) {
  (2 * exp(1/2 * (sqrt(lambda^2 + 6 * lambda + 1) - lambda - 3) * x))/sqrt(lambda^2 + 6 * lambda + 1) - (2 * exp(1/2 * (-sqrt(lambda^2 + 6 * lambda + 1) - lambda - 3) * x))/sqrt(lambda^2 + 6 * lambda + 1)
}

# Testing when lambda = 0
# x <- seq(0, 10, length.out = 100)
#plot(x, f(0,x), type = "l")
#lines(x, 2 * (exp(-x) - exp(-2* x)), type = "l", col = "red")

# integrate(function(x) 2 * (exp(-x) - exp(-2* x)), lower = 0, upper = 100)

contact_density <- function(df, title) {
  ggplot(df) +
    geom_line(aes(x = t, y = ft, color = as.factor(λ))) +
    #  facet_wrap(~ λ, nrow = 2, labeller=label_both) +
    labs(title = title, color="λ") +
    ylab("") +
    xlab("time") +
    theme_minimal(base_size = 18) +
    theme(panel.spacing = unit(2, "lines"))
}

#' Q matrix for complete contact process with 2 nodes
#' Projected to the number of ones in the process
#' onlyTrans=TRUE remove the absorbing states
QC2 <- function(lambda, onlyTrans = FALSE) {
  A <- matrix(c(
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

# plot(t, hazard_phtype(t, pi2, QC2(lambda = .001, onlyTrans = TRUE)), type = "l")

df <- data.frame(do.call(rbind, lapply(lambdas, function(l) cbind(λ = l, t = t, ft = f(l, t), ft2 =  dphtype(t, pi2, QC2(l,onlyTrans = TRUE))))))

contact_density(df, "Complete contact process 2 nodes")

ggsave(filename = "complete_2_contact_phase_densities.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

# Check that it integrates to 1 (e.g is a density)
g <- function(x) {
  f(2, x)
}

integrate(g, lower = 0, upper = Inf)

c2_var <- function(L) {
  (L^2 + 6 * L + 5) / 4
}

# Double check that algebra calculation of variance is the same
# as the one computed numerically

assertthat::are_equal(c2_var(2), var_phtype(pi2, QC2(2, onlyTrans = TRUE)))
assertthat::are_equal(c2_var(3), var_phtype(pi2, QC2(3, onlyTrans = TRUE)))
assertthat::are_equal(c2_var(15), var_phtype(pi2, QC2(15, onlyTrans = TRUE)))

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

df <- data.frame(do.call(rbind, lapply(lambdas, function(l) cbind(λ = l, t = t, ft = dphtype(t, pi3, QC3(l,onlyTrans = TRUE))))))

contact_density(df, "Complete contact process 3 nodes")

ggsave(filename = "complete_3_contact_phase_densities.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

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

df <- data.frame(do.call(rbind, lapply(lambdas, function(l) cbind(λ = l, t = t, ft = dphtype(t, pi5, QS4(l,onlyTrans = TRUE))))))

# lambda <- 2
# plot(t, hazard_phtype(t, c(0,0,0,0,1), QS4(2,onlyTrans = TRUE)), type = "l")

contact_density(df, "Cycle contact process 4 nodes")

ggsave(filename = "cycle_4_contact_phase_densities.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

QL3 <- function(lambda, onlyTrans=FALSE) {
 A <- matrix(c(
  -3, 2, 1, 0, 0, 0,
  lambda, -(lambda + 2), 0, 1, 1, 0,
  lambda, 0, -(lambda + 1), 1, 0, 0,
  0, lambda, 0, -(lambda + 1), 0, 1,
  0, 2 * lambda, 0, 0, -(2 * lambda + 1), 1,
  0, 0, 0, 0, 0, 0
 ), byrow = TRUE, ncol = 6)

 if(onlyTrans) A[-ncol(A), -ncol(A)]
 else A
}

df <- data.frame(do.call(rbind, lapply(lambdas, function(l) cbind(λ = l, t = t, ft = dphtype(t, pi5, QL3(l,onlyTrans = TRUE))))))

contact_density(df, "Contact process 3 nodes on 1D lattice")

ggsave(filename = "lattice_3_contact_phase_densities.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

# Compute the means and variances
mean_lambdas <- seq(.1, 15, length.out = 50)
res_mean_c2 <-unlist(lapply(mean_lambdas, function(l) mphtype(1, pi2, QC2(l ,onlyTrans = TRUE))))
res_mean_c3 <-  unlist(lapply(mean_lambdas, function(l) mphtype(1, pi3, QC3(l ,onlyTrans = TRUE))))
res_mean_s4 <- unlist(lapply(mean_lambdas, function(l) mphtype(1, pi5, QS4(l ,onlyTrans = TRUE))))
res_mean_l3 <- unlist(lapply(mean_lambdas, function(l) mphtype(1, pi5, QL3(l ,onlyTrans = TRUE))))

res_mean <- data.frame(lambda = mean_lambdas, C2 = res_mean_c2, C3 = res_mean_c3,
                       L3 = res_mean_l3,
                       S4 = res_mean_s4)
res_mean_long <- gather(res_mean, model, ev, C2:L3)

g1 <- ggplot(res_mean_long) +
 geom_line(aes(x = lambda, y = ev, color = model)) +
 ylab("expected time") +
 xlab("λ") +
 theme_minimal(base_size = 18)

# Add S4 to the results
res_mean_long_s4 <- gather(res_mean, model, ev, C2:S4)

g2 <-ggplot(res_mean_long_s4) +
 geom_line(aes(x = lambda, y = ev, color = model)) +
 ylab("expected time") +
 xlab("λ") +
 theme_minimal(base_size = 18)

#grid.arrange(g1, g2, nrow = 1, top = "Comparison of expected value for various contact processes")
ggarrange(g2, g1, nrow = 1, common.legend = TRUE, legend="bottom")

# Show without S4
ggsave(plot = g1, filename = "ev_phase_comparison_3.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

# Include S4
ggsave(plot = g2, filename = "ev_phase_comparison_4.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

var_lambdas <- seq(.1, 15, length.out = 50)
res_var_c2 <-unlist(lapply(mean_lambdas, function(l) var_phtype(pi2, QC2(l ,onlyTrans = TRUE))))
res_var_c3 <-  unlist(lapply(mean_lambdas, function(l) var_phtype(pi3, QC3(l ,onlyTrans = TRUE))))
res_var_s4 <- unlist(lapply(mean_lambdas, function(l) var_phtype(pi5, QS4(l ,onlyTrans = TRUE))))
res_var_l3 <- unlist(lapply(mean_lambdas, function(l) var_phtype(pi5, QL3(l ,onlyTrans = TRUE))))

res_var <- data.frame(lambda = mean_lambdas, C2 = res_var_c2, C3 = res_var_c3,
                       L3 = res_var_l3,
                       S4 = res_var_s4)
res_var_long <- gather(res_var, model, var, C2:L3)

ggplot(res_var_long) +
 geom_line(aes(x = lambda, y = var, color = model)) +
 ylab("variance") +
 xlab("λ") +
 theme_minimal(base_size = 18)

