library(interparsys)

set.seed(13)
v <- vm_dual_sim(n = 4, pretty = TRUE)
g1 <- v$dual_plot
g2 <- v$example_path_plot

png(here("../figures/voter_model_dual.png"))
ggarrange(g1, g2, nrow = 1, common.legend = TRUE, legend="bottom")
dev.off()
