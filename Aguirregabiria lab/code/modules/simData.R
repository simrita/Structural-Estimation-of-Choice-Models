sim_data <- function(l, num.obs = 10^4, beta = 0.95) {
  util <- function(i, s, q) {
    l$pr * s - l$pw * q * (l$Q - i + s) - l$alpha * (i - s) - l$eta_q * q
  }

  V <- value_fn_iteration(util, l$Q)

  sim_data <- data.frame(
    time = c(1:num.obs),
    i = rep(NA, num.obs),
    s = rep(NA, num.obs),
    q = rep(NA, num.obs)
  )
  i_0 <- 10
  for (t in 1:10^4) {
    d <- rlnorm(1, l$mu, l$sigma)
    sim_data$s[t] <- min(i_0, d)
    sim_data$i[t] <- i_0
    sim_data$q[t] <- which.max(
      c(
        util(i_0, sim_data$s[t], 0) + beta * V(i_0 - sim_data$s[t]) +
          fExtremes::rgev(1, xi = 0, mu = 0, beta = 1),
        util(i_0, sim_data$s[t], 1) + beta * V(l$Q) +
          fExtremes::rgev(1, xi = 0, mu = 0, beta = 1)
      )
    ) - 1
    i_0 <- i_0 - sim_data$s[t] + sim_data$q[t] * (l$Q - i_0 + sim_data$s[t])
  }

  sim_data
}
