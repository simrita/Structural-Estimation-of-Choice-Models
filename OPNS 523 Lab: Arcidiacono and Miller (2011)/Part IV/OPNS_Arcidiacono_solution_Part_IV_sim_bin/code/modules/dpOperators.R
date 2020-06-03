#############Notes:
#calc_csv
  #Look how v.0 is defined over multiple lines
  #Use "order_by" and "default" options in lead(V, order_by = x, default = last(V, x))
  #Use "order_by" option in first(V, order_by = x)

#calc_log_ccp
  #Never refers to specific action (code could accomodate 1,000 actions)
  #Use log ccps, because probability can go to zero

#calc_log_duration_prob:
  #cumsum() calculates the log probability of survival up until that point
  #Use "default" and "order_by" options in lag(p.0, default=1, order_by=x)

#calc_value_function
  #bellman_operator
    #Never refers to specific action
    #Drawback:  melt steps incur fixed overhead, so this method is a bit slower
    #Avoid Inf: V = max(value) + log(sum(exp(value - max(value))))
    #Relative value iteration: V - first(V, order_by = x)

  #Use map_df to translate l$s to state space
  #This is basically only time you use a "while" loop
#############

calc_csv <- function(value.fn, l) { #Maps value functions to choice specific value functions
  value.fn %>% 
    group_by(s) %>% 
    mutate(
      v.0 = lead(V, order_by = x, default = last(V, x)),
      v.0 = l$theta[1]*s - l$theta[2]*x + l$beta * v.0,
      v.1 = l$beta * first(V, order_by = x)
    ) %>%
    ungroup %>% 
    select(-V)
}

calc_log_ccp <- #Maps choice specific value functions to log ccps
  . %>% 
  melt(c('s', 'x')) %>% 
  group_by(s, x) %>% 
  mutate(
    variable = str_replace(variable, 'v', 'p'),
    value = value - max(value),
    value = value - log(sum(exp(value)))
  ) %>% 
  dcast(
    s + x ~ variable,
    value.var = 'value'
  )

calc_log_duration_prob <- #Maps log CCPs to log duration probabilities
  . %>% 
  group_by(s) %>%
  mutate(log.like = p.1 + cumsum(lag(p.0, default=0, order_by=x))) %>%
  ungroup %>% 
  select(s, x, log.like)

calc_value_function <- function(l, epsilon = c(10^-6, 10^-6)){ 
  bellman_operator <- #Maps value function to value functions
    . %>% 
    calc_csv(l) %>% 
    melt(c('s', 'x')) %>% 
    group_by(s, x) %>% 
    summarise(V = max(value) + log(sum(exp(value - max(value))))) %>% 
    group_by(s) %>% 
    mutate(V = V - first(V, order_by = x)) %>% 
    ungroup
  
  V. <-
    l$intercepts$s %>% 
    map_df(~{
      tibble(
        s = ., #set state space:
        x = seq(0, l$theta[1] / l$theta[2] * . + log(epsilon[1])/log(1/2))
      )
    }) %>% 
    mutate(V = 0)
  
  epsilon <- epsilon[2]
  delta <- 1
  while(delta > epsilon){
    V <- bellman_operator(V.)
    delta <- max(abs(V$V-V.$V))
    V. <- V
    print(delta)
  }
  
  V
}
