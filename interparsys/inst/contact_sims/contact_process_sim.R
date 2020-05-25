library(interparsys)

set.seed(26)
png(here("figures/contact_simulation_torus_25.png"))
par(mar=c(1, 1, 1, 1), mfrow=c(2,2))
simulate_contact(lambda = 4, n = 50, torus = TRUE, times = c(5, 10, 30), init_config = "one")
dev.off()

set.seed(26)
png(here("figures/contact_simulation_torus_25_below_crit.png"))
par(mar=c(1, 1, 1, 1), mfrow=c(2,2))
simulate_contact(lambda = 0.25, n = 50, torus = TRUE, times = c(1, 3, 10), init_config = "all")
dev.off()
