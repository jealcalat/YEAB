#
# t_ev <- rho_tr_tand_xs[rho_tr_tand_xs$sesion == 27,]
# t_ev <- matrix(c(t_ev$cum_dt,t_ev$evento),ncol = 2)
#
# r = 1; R = 2; sec_mult = 1000; bin_res = 200
#
# t_ev %<>% as.matrix()

rR_corr_boot <- function(t_ev, r, R, sec_mult, bin_res, B) {

  # The max time at which subject get a reinforfer
  max_t <- max(t_ev[t_ev[, 2] == R, 1]) * sec_mult

  # vector of events
  v_events <- c(r, R)

  # Create matrix of 3 columns to get bins and frecuencies of events in those bins
  m_rR <-
    seq(0, max_t, bin_res) %>%
    seq_along() %>%
    data.frame(
      bin_r = .,
      r = NA,
      R = NA
    )

  for (j in 1:2) {
    e <- v_events[j]
    event_bins <- t_ev[which(t_ev[, 2] == e), 1] * sec_mult %>%
      get_bins(., 1, max_t, bin_res)


    for (i in 1:nrow(m_rR)) {
      sum <- sum(m_rR[i, 1] == event_bins)
      m_rR[i, j + 1] <- sum # j + 1 because m_rR have 3 columns
    }
  }
  n <- nrow(m_rR)
  B <- 10000

  pearson <- function(df, i = c(1:n)) {
    df2 <- df[i, ]
    cor(df2[, 2], df2[, 3])
  }

  cor.boot <- boot(data = m_rR, statistic = pearson, R = B)

  rho <- list(
    rho = mean(cor.boot$t, na.rm = T),
    low = quantile(cor.boot$t, probs = c(0.025, 0.975), na.rm = T)[1],
    upp = quantile(cor.boot$t, probs = c(0.025, 0.975), na.rm = T)[2]
  )
  rho
}
