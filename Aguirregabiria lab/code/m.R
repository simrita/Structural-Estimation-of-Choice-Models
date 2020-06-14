source('header.R')

list(
  mu = 1,
  sigma = 1,
  pr = 5,
  pw = 1,
  alpha = 0.3,
  eta_q = 4,
  Q = 20
) %>% 
  sim_data %T>%
  saveRDS('../variables/sim_data.rds')

estimates <-
  read_rds('../variables/sim_data.rds') %>% 
  estimate_model(.)
