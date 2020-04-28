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

a1 <- c(1,0,0)
a2 <- c(0,1,0)
a3 <- c(0,0,1)
a4 <- (1/3) * c(1,1,1)

alphas <-list(a1, a2, a3, a4)
t <- seq(0, 7.5, length.out = 1000)
dphtype(t, p1, QS4(onlyTrans = TRUE))

voter_density <- function(df, title) {

}

mphtype(1, c(1,0), QC4())
mphtype(1, a2, QS4())
mphtype(1, a3, QS4())

df <- do.call(rbind, lapply(alphas, function(a) data.frame(alph = paste(a, collapse = ","),
                                         t = t,
                                         ft = dphtype(t, a, QS4(onlyTrans = TRUE)))))

ggplot(df) +
  geom_line(aes(x = t, y = ft, color = alph)) +
  ylab("") +
  xlab("time") +
  theme_minimal(base_size = 18) +
  theme(panel.spacing = unit(2, "lines")) +
  scale_color_discrete(name="alpha",
                       breaks=levels(df$alph),
                       labels=c("(1,0,0)","(0,1,0)", "(0,0,1)", "1/3"))
