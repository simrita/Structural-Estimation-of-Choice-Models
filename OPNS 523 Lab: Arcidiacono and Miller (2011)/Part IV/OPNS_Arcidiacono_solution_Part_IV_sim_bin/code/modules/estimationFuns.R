##### Notes
# simData:
# Use %T>% to save intermediary result
# Use sample_n to sample by likelihood: calculate the likelihood only once
# If you simulate each person individually, your code will take hours.
# Sum across mileage levels to express the data in most parsimonious fashion
##########

sim_data <- function(l, num.obs = 10^6) { # Create simulation data
  l %>%
    calc_value_function() %T>%
    saveRDS("../variables/value_fn.rds") %>%
    calc_csv(l) %>%
    calc_log_ccp() %>%
    calc_log_duration_prob() %>%
    inner_join(l$intercepts) %>%
    mutate(prob = pi.s * exp(log.like)) %>%
    sample_n(
      size = num.obs,
      weight = prob,
      replace = TRUE
    ) %>%
    count(s, x)
}

reduced_CCP <- function(simulated_data) {
  set <- simulated_data %>%
    group_by(s) %>%
    mutate(prob.1 = n / sum(n)) %>%
    mutate(prob.0 = 1 - n / sum(n)) %>%
    mutate(p1 = exp(log(prob.1) - log(1-cumsum(lag((prob.1), default = 0, order_by = x))))) %>%
    ungroup() %>%
    select(s, x, p1) %>%
    rename(prob = p1)

  set2 <- rbind(set %>% mutate(a = rep(1, length(n))), set %>% mutate(a = rep(0, length(n))) %>% mutate(prob = 1 - prob)) %>%
    select(s, x, a, prob) %>%
    arrange(x)
  rbind(set2, set2 %>% group_by(s) %>% arrange(s, x) %>% filter(x == max(x)) %>% mutate(x = x + 1) %>% ungroup())
}
log_likelihood <- function(theta) {
  l <- list(
    theta = theta,
    beta = .99
  )
  simulated_data %>%
    reduced_CCP() %>%
    arrange(s, x) %>%
    filter(a == 1) %>%
    group_by(s) %>%
    arrange(x) %>%
    mutate(V = log(prob[1]) - log(prob)) %>%
    ungroup() %>%
    select(-a, -prob) %>%
    calc_csv(l) %>%
    calc_log_ccp() %>%
    calc_log_duration_prob() %>%
    left_join(simulated_data) %>%
    summarize(sum(na.omit(n * log.like)))
}
optima <- function(var) {
  obj <- optim(par = unlist(var), fn = log_likelihood, control = list(fnscale = -1), method = "L-BFGS-B", lower = c(0, 0))
  setNames(data.frame(t(c(obj$par, obj$value))), c("theta_1", "theta_2", "log_like"))
}
