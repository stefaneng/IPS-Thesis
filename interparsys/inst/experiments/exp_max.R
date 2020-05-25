
eq_rates <- c(1/2, 1/2)
alt_rates <- c(3/4, 1/4)
alt_rates2 <- c(7/8, 1/8)
alt_rates3 <- c(31/32, 1/32)

mean(apply(sapply(eq_rates, function(r) rexp(1000, r)), 1, max))

mean(apply(sapply(alt_rates, function(r) rexp(1000, r)), 1, max))

mean(apply(sapply(alt_rates2, function(r) rexp(1000, r)), 1, max))

mean(apply(sapply(alt_rates3, function(r) rexp(1000, r)), 1, max))

