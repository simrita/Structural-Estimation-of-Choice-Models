source('header.R')

list(
  theta = c(10, 0.01),
  s.val = c(1, 4),
  pi.s = .25,
  beta = .99,
  epsilon = 10^-3 #value for determining x.M
) %>% 
  sim_data