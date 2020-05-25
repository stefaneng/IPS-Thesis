library(interparsys)

# Complete graph with split config
set.seed(13)
png(here("../figures/voter_simulation_1d_complete_split_100.png"))
plot_1d(n = 100, max_iter = 3e4, complete_graph = TRUE, init_config = "split")
dev.off()

set.seed(13)
png(here("../figures/voter_simulation_1d_300.png"))
plot_1d(n = 300, max_iter = 3e4)
dev.off()

set.seed(262)
png(here("../figures/voter_simulation_torus_50.png"))
par(mar=c(1, 1, 1, 1), mfrow=c(2,2))
simulate_voter(n = 50, times = c(5,30,300))
dev.off()

set.seed(262)
png(here("../figures/voter_simulation_torus_100.png"))
par(mar=c(1, 1, 1, 1), mfrow=c(2,2))
simulate_voter(n = 100, times = c(25,250,750))
dev.off()


# set.seed(27)
# # Create an animation of the voter model
# #ani.record(reset = TRUE)
# #oopts <- ani.options(interval = 0.1)
# img_dir <- here("gifs/voter_images/")
# png(paste0(img_dir, "voter_sim%04d.png"))
# par(mar=c(0, 0, 0, 0))
# simulate_voter(n = 50, times = seq(0, 500, by = 1), title = FALSE, animate = TRUE)
# # ani.replay()
# #saveGIF(ani.replay(), movie.name = paste0(here("gifs/test.gif")), )
# dev.off()
# system(paste0("convert -delay 10 ", img_dir, "*.png ", here("gifs/"), "voter_sim.gif"))
# file.remove(list.files(path = img_dir, pattern=".png", full.names = TRUE))
