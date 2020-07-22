library(interparsys)
library(ggplot2)
#library(gridExtra)
library(ggpubr)
library(actuar)
library(tidyr)
library(dplyr)
library(purrr)

cbPalette <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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

contact_survival <- function(df, title = "") {
  ggplot(df) +
    geom_line(aes(x = t, y = survival, color = as.factor(λ))) +
    #  facet_wrap(~ λ, nrow = 2, labeller=label_both) +
    labs(title = title, color="λ") +
    ylab("survival probability") +
    xlab("time") +
    theme_minimal(base_size = 18) +
    theme(panel.spacing = unit(2, "lines"))
}

contact_density <- function(df, title = "") {
  ggplot(df) +
    geom_line(aes(x = t, y = ft, color = as.factor(λ))) +
    #  facet_wrap(~ λ, nrow = 2, labeller=label_both) +
    labs(title = title, color="λ") +
    ylab("density") +
    xlab("time") +
    theme_minimal(base_size = 18) +
    theme(panel.spacing = unit(2, "lines"))
}

contact_hazard <- function(df, title = "") {
  ggplot(df) +
    geom_line(aes(x = t, y = hazard, color = as.factor(λ))) +
    #  facet_wrap(~ λ, nrow = 2, labeller=label_both) +
    labs(title = title, color="λ") +
    ylab("hazard") +
    xlab("time") +
    theme_minimal(base_size = 18) +
    theme(panel.spacing = unit(2, "lines"))
}

#' @param Q function that takes in one parameter lambda and returns a Q matrix
create_df <- function(lambdas, Q, pi, t = seq(0, 10, length.out = 1000)) {
  # Should add pi as a vector..
  # Then can do expand.grid(lambdas = c(1,2), pi = list(c(1,0,0), c(0,1,0)))
  # And apply to the columns
  data.frame(
    do.call(rbind.data.frame,
            lapply(lambdas, function(l) {
              cbind.data.frame(
                λ = l, t = t, ft =  dphtype(t, pi, Q(l,onlyTrans = TRUE)),
                survival = pphtype(t, pi, Q(l,onlyTrans = TRUE), lower.tail = F),
                hazard = hazard_phtype(t, pi, Q(l,onlyTrans = TRUE)))
            })
    )
  )
}

create_df_pi <- function(Q, pi, lambda, t = seq(0, 10, length.out = 1000)) {
  # Should add pi as a vector..
  # Then can do expand.grid(lambdas = c(1,2), pi = list(c(1,0,0), c(0,1,0)))
  # And apply to the columns
  data.frame(
    do.call(rbind.data.frame,
            lapply(pi, function(p) {
              p_string <- paste0("(", paste(p, collapse = ","), ")")
              cbind.data.frame(
                λ = lambda, pi = p_string, t = t, ft =  dphtype(t, p, Q(lambda, onlyTrans = TRUE)),
                hazard = hazard_phtype(t, p, Q(lambda,onlyTrans = TRUE)))
            })
    )
  )
}

#' Creates a dataframe starting in each initial state
create_hazard_init_df <- function(Q, t = seq(0, 10, length.out = 1000), lambda = 1) {
  q <- Q(lambda, onlyTrans = TRUE)
  n <- nrow(q)
  inits <- as.data.frame(diag(n))
  lapply(inits, function(pi) hazard_phtype(t, pi, q))
}

# plot(t, hazard_phtype(t, pi2, QK2(lambda = .001, onlyTrans = TRUE)), type = "l")

k2_df <- create_df(lambdas, QK2, pi2)
#k2_df <- data.frame(do.call(rbind, lapply(lambdas, function(l) cbind(λ = l, t = t, ft = f(l, t), ft2 =  dphtype(t, pi2, QK2(l,onlyTrans = TRUE)),
#                                                                   hazard = hazard_phtype(t, pi2, QK2(l,onlyTrans = TRUE))))))

contact_density(k2_df)
ggsave(filename = "complete_2_contact_phase_densities.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)


contact_survival(k2_df)
ggsave(filename = "complete_2_contact_survival.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

# contact_hazard(k2_df, "")
# ggsave(filename = "complete_2_contact_hazard.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

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

assertthat::are_equal(c2_var(2), var_phtype(pi2, QK2(2, onlyTrans = TRUE)))
assertthat::are_equal(c2_var(3), var_phtype(pi2, QK2(3, onlyTrans = TRUE)))
assertthat::are_equal(c2_var(15), var_phtype(pi2, QK2(15, onlyTrans = TRUE)))

k3_df <- create_df(lambdas, QK3, pi3, t = seq(0, 30, length.out = 1000))

contact_density(k3_df)
ggsave(filename = "complete_3_contact_phase_densities.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

contact_survival(k3_df)
ggsave(filename = "complete_3_contact_survival.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

#contact_hazard(k3_df)
#ggsave(filename = "complete_3_contact_hazard.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

# Get the moments
# mphtype(1, pi, QK3(3,onlyTrans = TRUE))

s4_df <- create_df(lambdas, QS4, pi5)

# lambda <- 2
# plot(t, hazard_phtype(t, c(0,0,0,0,1), QS4(2,onlyTrans = TRUE)), type = "l")

contact_density(s4_df)
ggsave(filename = "cycle_4_contact_phase_densities.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

# contact_hazard(s4_df)
# ggsave(filename = "cycle_4_contact_hazard.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

l3_df <- create_df(lambdas, QL3, pi5)

contact_density(l3_df)
ggsave(filename = "lattice_3_contact_phase_densities.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

# contact_hazard(l3_df)
# ggsave(filename = "lattice_3_contact_hazard.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

# Compute the means and variances
mean_lambdas <- seq(.1, 15, length.out = 50)
res_mean_c2 <-unlist(lapply(mean_lambdas, function(l) mphtype(1, pi2, QK2(l ,onlyTrans = TRUE))))
res_mean_c3 <-  unlist(lapply(mean_lambdas, function(l) mphtype(1, pi3, QK3(l ,onlyTrans = TRUE))))
res_mean_s4 <- unlist(lapply(mean_lambdas, function(l) mphtype(1, pi5, QS4(l ,onlyTrans = TRUE))))
res_mean_l3 <- unlist(lapply(mean_lambdas, function(l) mphtype(1, pi5, QL3(l ,onlyTrans = TRUE))))

res_mean <- data.frame(lambda = mean_lambdas, K2 = res_mean_c2, K3 = res_mean_c3,
                       L3 = res_mean_l3,
                       S4 = res_mean_s4)
res_mean_long <- gather(res_mean, model, ev, K2:L3)

g1 <- ggplot(res_mean_long) +
 geom_line(aes(x = lambda, y = ev, color = model)) +
 ylab("expected time") +
 xlab("λ") +
 theme_minimal(base_size = 18)

# Add S4 to the results
res_mean_long_s4 <- gather(res_mean, model, ev, K2:S4)

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
res_var_c2 <-unlist(lapply(mean_lambdas, function(l) var_phtype(pi2, QK2(l ,onlyTrans = TRUE))))
res_var_c3 <-  unlist(lapply(mean_lambdas, function(l) var_phtype(pi3, QK3(l ,onlyTrans = TRUE))))
res_var_s4 <- unlist(lapply(mean_lambdas, function(l) var_phtype(pi5, QS4(l ,onlyTrans = TRUE))))
res_var_l3 <- unlist(lapply(mean_lambdas, function(l) var_phtype(pi5, QL3(l ,onlyTrans = TRUE))))

res_var <- data.frame(lambda = mean_lambdas, K2 = res_var_c2, K3 = res_var_c3,
                       L3 = res_var_l3,
                       S4 = res_var_s4)
res_var_long <- gather(res_var, model, var, K2:L3)

ggplot(res_var_long) +
 geom_line(aes(x = lambda, y = var, color = model)) +
 ylab("variance") +
 xlab("λ") +
 theme_minimal(base_size = 18)

# Hazard functions for various initial distributions
contact_hazard_pi <- function(df, color_labels = NULL, title = "") {
  g <- ggplot(df) +
    geom_line(aes(x = t, y = hazard, color = as.factor(pi))) +
    labs(title = title, color="Start State") +
    ylab("hazard") +
    xlab("time") +
    theme_minimal(base_size = 18) +
    theme(panel.spacing = unit(2, "lines"))

  if(!is.null(color_labels)) {
    g + scale_color_manual(labels = color_labels, values = cbPalette[2:(length(color_labels) + 2)])
  } else {
    g
  }
}

k2_df_pi <- create_df_pi(QK2, list(c(1,0), c(0,1)), lambda = 1, t = seq(0, 3, length.out = 1000))
contact_hazard_pi(k2_df_pi, color_labels = c(2,1))
ggsave(filename = "complete_2_contact_hazard_pi.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

k3_df_pi <- create_df_pi(QK3, list(c(1,0,0), c(0,1,0), c(0,0,1)), lambda = .25, t = seq(0, 5, length.out = 1000))
contact_hazard_pi(k3_df_pi, color_labels = c(3,2,1))
ggsave(filename = "complete_3_contact_hazard_pi.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

s4_df_pi <- create_df_pi(QS4, as.list(as.data.frame(diag(5))), lambda = 1, t = seq(0, 4, length.out = 1000))
contact_hazard_pi(s4_df_pi)
ggsave(filename = "cycle_4_contact_hazard_pi.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

l3_df_pi <- create_df_pi(QL3, as.list(as.data.frame(diag(5))), lambda = 1, t = seq(0, 4, length.out = 1000))
contact_hazard_pi(l3_df_pi)
ggsave(filename = "lattice_3_contact_hazard_pi.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)

