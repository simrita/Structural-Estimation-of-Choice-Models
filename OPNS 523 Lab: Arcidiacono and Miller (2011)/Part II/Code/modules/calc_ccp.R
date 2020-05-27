cal_prob <- function(dat) {
    vec.val <- dat$val
    dat %>% group_by(choice.id) %>% 
      mutate(prob =  1/sum(exp(vec.val - val))) %>% ungroup()
  }

calc_CCP <- . %>% 
              group_by(i) %>% 
                do(cal_prob(.)) %>% 
                  mutate(sum.prob = sum(prob)) %>% 
                    select(i, choice.id, val, prob, sum.prob) %>% 
              ungroup()
