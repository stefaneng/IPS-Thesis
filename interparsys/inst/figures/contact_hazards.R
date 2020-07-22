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

create_df <- function(lambdas, Q, pi) {
  data.frame(
    do.call(rbind,
            lapply(lambdas, function(l) {
              cbind(
                λ = l, t = t, ft =  dphtype(t, pi, Q(l,onlyTrans = TRUE)),
                hazard = hazard_phtype(t, pi, Q(l,onlyTrans = TRUE)))
            })
    )
  )
}