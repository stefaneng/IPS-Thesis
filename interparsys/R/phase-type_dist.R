library(ggplot2)

f <- function(lambda, x) {
 (2 * exp(1/2 * (sqrt(lambda^2 + 6 * lambda + 1) - lambda - 3) * x))/sqrt(lambda^2 + 6 * lambda + 1) - (2 * exp(1/2 * (-sqrt(lambda^2 + 6 * lambda + 1) - lambda - 3) * x))/sqrt(lambda^2 + 6 * lambda + 1)
}

t <- seq(0, 10, length.out = 1000)
lambdas <- c(1, 2, 5, 15)

df <- data.frame(do.call(rbind, lapply(lambdas, function(l) cbind(λ = l, t = t, ft = f(l, t)))))

ggplot(df) +
 geom_line(aes(x = t, y = ft)) +
 facet_wrap(~ λ, nrow = 2, labeller=label_both) +
 labs(title = "Densities of time to state 0 for varying λ") +
 ylab("") +
 xlab("time") +
 theme_minimal()

ggsave(filename = "contact_phase_densities.png", path = here::here("figures"))

# Check that it integrates to 1 (e.g is a density)
g <- function(x) {
 f(2, x)
}

integrate(g, lower = 0, upper = Inf)

