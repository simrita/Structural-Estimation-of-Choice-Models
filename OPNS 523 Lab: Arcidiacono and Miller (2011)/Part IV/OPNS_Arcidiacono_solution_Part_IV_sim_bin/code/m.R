source("header.R")
############## With 10^6 Observations ##########################################
set.seed(2022)
list(
  theta = c(5, 0.01),
  intercepts =
    tibble(
      s = c(1, 2),
      pi.s = c(.25, .75)
    ),
  beta = .99
) %>%
  sim_data(num.obs = 10^6) %T>%
  saveRDS("../variables/sim_data.rds")

simulated_data <- readRDS("../variables/sim_data.rds")
CCP_estimates <- simulated_data %>%
  reduced_CCP() %>%
  arrange(s, x)
CCP_estimates
Values <- CCP_estimates %>%
  filter(a == 1) %>%
  group_by(s) %>%
  arrange(x) %>%
  mutate(V = log(prob[1]) - log(prob)) %>%
  ungroup() %>%
  select(-a, -prob)
Values
suppressMessages(optima(c(10, 2))) %T>% saveRDS("../variables/estimates_big_data.rds")
suppressMessages(optima(c(1000, 1000)))

############## With 10^3 Observations ##########################################
set.seed(2022)
list(
  theta = c(5, 0.01),
  intercepts =
    tibble(
      s = c(1, 2),
      pi.s = c(.25, .75)
    ),
  beta = .99
) %>%
  sim_data(num.obs = 10^3) %T>%
  saveRDS("../variables/sim_data.rds")

simulated_data <- readRDS("../variables/sim_data.rds")
CCP_estimates <- simulated_data %>%
  reduced_CCP() %>%
  arrange(s, x)
CCP_estimates
Values <- CCP_estimates %>%
  filter(a == 1) %>%
  group_by(s) %>%
  arrange(x) %>%
  mutate(V = log(prob[1]) - log(prob)) %>%
  ungroup() %>%
  select(-a, -prob)
Values
suppressMessages(optima(c(10, 2))) %T>% saveRDS("../variables/estimates_small_data.rds")
suppressMessages(optima(c(1000, 1000)))
