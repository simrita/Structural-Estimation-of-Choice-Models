valueIteration <- function(dat,tol.conv=1e-7) {
  curr.tol = 9e9
  while(curr.tol > tol.conv) {
    dat <- dat %>% arrange(a,x) %>% 
            mutate(val = util + beta*EV.curr) %>% 
              group_by(x) %>% 
               mutate(inclu_val=log(sum(exp(val)))) %>% 
               mutate(inclu_val = ifelse(inclu_val==Inf,
                                  max(val),
                                  inclu_val)) %>%
               mutate(inclu_val = ifelse(inclu_val==-Inf,
                                  min(val),
                                  inclu_val)) %>% 
             ungroup()
    inclu_val_0 <- dat %>% filter(a==1,x==0) %>% select(inclu_val) %>% unique()
    EV.new <- dat %>% mutate(EV.new = ifelse(a==0, lead(inclu_val,order_by=x),
                                                 inclu_val_0)) %>% 
                      mutate(EV.new = ifelse(is.na(EV.new), inclu_val, EV.new)) %>% 
                        arrange(a,x) %>% 
                          select(EV.new) %>% unlist() 
    curr.tol <- max(EV.new - as.numeric(dat$EV.curr))
    dat$EV.curr <- EV.new
  }
  cal_prob <- function(dat) {
    vec.val <- dat$val
    dat %>% group_by(a) %>% 
      mutate(prob =  1/sum(exp(vec.val - val))) %>% ungroup()
  }
  dat %>%  
    group_by(x) %>% 
    do(cal_prob(.)) %>% 
    ungroup() %>% 
     filter(a==1) %>% 
      select(x,prob)
}