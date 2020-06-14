loglik_demand <- function(par, sim_data) {
  sim_data %>%
    mutate(
      logprob =
        ifelse(s < i,
          log(dnorm(log(s), par[1], par[2])),
          log(1 - dnorm(log(s), par[1], par[2]))
        )
    ) %>%
    summarise(-sum(logprob))
}

loglik_inventory <- function(par, model, sim_data, est_demand_par, beta = .95) {
  Q <- max(sim_data$i)
  util <- function(i, s, q) {
    par[1] * s - q * (Q - i + s) - exp(par[2]) * (i - s) - exp(par[3]) * q
  }
  f <- function(x) {
    integrand <- function(s) {
      val <- util(x, s, 1) - log(predict(model, newdata = data.frame(i = x, s = s)))
      dlnorm(s, est_demand_par[1], est_demand_par[2]) * val
    }

    (util(x, x, 1) - log(
      predict(model,
        newdata = data.frame(i = x, s = x)
      )
    )
    ) *
      (1 - plnorm(
        x, est_demand_par[1],
        est_demand_par[2]
      )) +
      integrate( # interior part
        integrand,
        lower = 0,
        upper = x
      )$value
  }
  f_tilde <-
    ipol(
      val = Vectorize(f),
      intervals = c(0, Q),
      dims = 20
    )
  sim_data %>%
    mutate(
      prob1 =
        1 / (1 + exp(util(i, s, 0) +
          beta * f_tilde(i - s) -
          util(i, s, 1) -
          beta * f_tilde(Q))),
      prob0 = 1 - prob1,
      logprob = log(prob1 * q + prob0 * (1 - q))
    ) %>%
    summarise(-sum(logprob))
}

estimate_model <- function(.) {
  est_demand_par <- optim(c(2, 1), loglik_demand, sim_data = .)$par
  bw <- npregbw(formula = q ~ i - s, data = .)
  model <- npreg(bws = bw, gradients = TRUE)
  est_inventory_par <- optim(c(5, -1, 1),
    loglik_inventory,
    sim_data = .,
    model = model,
    est_demand_par = est_demand_par
  )$par
  data.frame(
    variables = c("mu", "sigma", "pr", "alpha", "eta_q"),
    value = c(
      est_demand_par,
      est_inventory_par[1],
      exp(est_inventory_par)[2],
      exp(est_inventory_par)[3]
    )
  ) %T>%
    saveRDS("../variables/estimates.rds")
}
