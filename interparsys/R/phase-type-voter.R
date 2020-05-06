library(actuar)
library(ggplot2)

QC4 <- function(onlyTrans = TRUE) {
  A <- matrix(c(
    -8/3, 8/3, 0,
    1, -2, 1,
    0, 0, 0
  ),
  byrow=TRUE,
  nrow=3
  )

  if(onlyTrans) A[-ncol(A), -ncol(A)]
  else A
}

QS4 <- function(onlyTrans = TRUE) {
 A <- matrix(c(
  -2, 1, 0, 1,
  2, -2, 0, 0,
  4, 0, -4, 0,
  0, 0, 0, 0
 ),
 byrow=TRUE,
 nrow=4
 )

 if(onlyTrans) A[-ncol(A), -ncol(A)]
 else A
}

a1_s4 <- c(1,0,0)
a2_s4 <- c(0,1,0)
a3_s4 <- c(0,0,1)
a4_s4 <- (1/3) * c(1,1,1)

alphas_s4 <-list(a1_s4, a2_s4, a3_s4, a4_s4)
t_s4 <- seq(0, 7.5, length.out = 1000)
dphtype(t_s4, p1, QS4(onlyTrans = TRUE))

mphtype(1, a2, QS4())
mphtype(1, a3, QS4())

a1_c4 <- c(1, 0)
a2_c4 <- c(0, 1)
a5_c4 <- c(1/2, 1/2)
alphas_c4 <- list(a1_c4, a2_c4, a5_c4)
t_c4 <- seq(0, 7, length.out = 1000)

mphtype(1, c(1, 0), QC4())

df_c4 <- do.call(rbind, lapply(alphas_c4, function(a) data.frame(alpha = paste(a, collapse = ","),
                                                                  t = t_c4,
                                                                  ft = dphtype(t_c4, a, QC4(onlyTrans = TRUE)))))

df_s4 <- do.call(rbind, lapply(alphas_s4, function(a) data.frame(alpha = paste(a, collapse = ","),
                                         t = t_s4,
                                         ft = dphtype(t_s4, a, QS4(onlyTrans = TRUE)))))

g_c4 <- ggplot(df_c4) +
  geom_line(aes(x = t, y = ft, color = alpha)) +
  ylab("density") +
  xlab("time") +
  theme_minimal(base_size = 18) +
  theme(panel.spacing = unit(2, "lines")) +
  scale_color_discrete(name="alpha",
                       breaks=levels(df_c4$alpha),
                       labels=c("(1,0)","(0,1)", "(1/2, 1/2)"))

ggsave(plot = g_c4, filename = "voter_density_c4.png", path = here::here("figures"), dpi = 320, units = "mm", width = 200)

g_s4 <- ggplot(df_s4) +
  geom_line(aes(x = t, y = ft, color = alpha)) +
  ylab("density") +
  xlab("time") +
  theme_minimal(base_size = 18) +
  theme(panel.spacing = unit(2, "lines")) +
  scale_color_discrete(name="alpha",
                       breaks=levels(df_s4$alpha),
                       labels=c("(1,0,0)","(0,1,0)", "(0,0,1)", "1/3"))

ggsave(plot = g_s4, filename = "voter_density_s4.png", path = here::here("figures"), dpi = 320, units = "mm", width = 200)

