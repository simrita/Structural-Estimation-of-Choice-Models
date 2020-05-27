sim_values <- . %>% data.frame(i=1:.$num.trials, 
                               x=rpois(.$num.trials,.$lambda)+1, 
                               u=runif(.$num.trials, .$df.min, .$df.max)) %>%
                      select(i,x,u) %>% 
                        merge(data.frame(i = rep(.$i, .$x),
                                       choice.id = sequence(.$x),
                                       u = rep(.$u,.$x))) %>% 
                          group_by(i, choice.id) %>% 
                            mutate(val=rt(1, df=u)) %>% 
                              ungroup()