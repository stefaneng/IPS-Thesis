library(interparsys)
library(ggplot2)
library(latex2exp)

set.seed(131)
n <- 5
rate <- 4
df <- data.frame(x = sort(rexp(n, rate= rate)), y = seq_len(n))
df$xend <- c(df$x[2:n], max(df$x + 1/16))
df$xstart <- c(.2, df$x[1:(n - 1)])

g <- ggplot(df) +
 geom_segment(aes(x = x, xend = xend, y = y, yend = y)) +
 geom_segment(aes(x = x, xend = x, y = 0, yend = y), linetype=2, color="grey") +
 geom_segment(aes(x = xstart, xend = x, y = 1/2, yend = 1/2),
              arrow = arrow(length = unit(0.02, "npc")),
              lineend = "round", linejoin = "bevel") +
 geom_text(aes(x = xstart + (x - xstart)/2, y = 1/4,
               label = TeX(sprintf("$X_{%d}$", y), output = "character")),
           parse = TRUE) +
 # Hacky to draw arrows on both sides: just draw a second segment
 geom_segment(aes(x = x, xend = xstart, y = 1/2, yend = 1/2),
              arrow = arrow(length = unit(0.02, "npc")),
              lineend = "round", linejoin = "bevel") +
 geom_point(aes(x = x, y = y)) +
 scale_x_continuous(
   breaks = c(0, df$x),
   labels = c(0, TeX(sprintf("$S_{%d}$", df$y)))
 ) +
 ylab("N(t)") +
 xlab("t") +
 coord_cartesian(xlim = c(.195, 0.64), clip="off", expand = FALSE) +
 theme_minimal(base_size = 18) +
 theme(axis.line = element_line(colour = "black"),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank(),
       panel.background = element_blank())

ggsave(plot = g, filename = "poisson_realization.png", path = here::here("../figures"), dpi = 320, units = "mm", width = 200)
