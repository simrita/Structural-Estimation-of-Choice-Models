cals_inclusive_values <- . %>% 
                            group_by(i) %>% 
                              mutate(inclu_val=log(sum(exp(val)))) %>% 
                                mutate(inclu_val = ifelse(inclu_val==Inf,
                                                          max(val),
                                                          inclu_val)) %>%
                                  mutate(inclu_val = ifelse(inclu_val==-Inf,
                                                            min(val),
                                                            inclu_val)) %>%
                                    select(i,inclu_val) %>% 
                                      unique() %>% 
                            ungroup() 