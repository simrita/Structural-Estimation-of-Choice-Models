sim_data <- function(params) {
  
x_M_1 = params$theta[1]*params$s.val[1]/params$theta[2]/2 + log(params$epsilon)/log(0.5)
x_M_2 = params$theta[1]*params$s.val[2]/params$theta[2]/2 + log(params$epsilon)/log(0.5)

rbind(
  left_join(expand.grid(id=1:(10^4*params$pi.s),x=c(0,1:x_M_1)), 
            data.frame(x=c(0,1:x_M_1), EV.curr=0, 
                       a=c(rep(0,x_M_1+1),rep(1,x_M_1+1)),
                       beta=params$beta,
                       x_M_1=x_M_1) %>% 
              mutate(util=ifelse(a==0,
                                 params$theta[1]*params$s.val[1]-params$theta[2]*x,
                                 0)) %>% 
              valueIteration(.)) %>% 
    mutate(a=rbernoulli(n(),prob)*1) %>% arrange(id,x) %>% 
    mutate(rowx=ifelse(a==1,x,x_M_1)) %>% 
    group_by(id) %>% 
    top_n(-(min(rowx)+1),x) %>%
    ungroup() %>% 
    select(id,x,a),
  left_join(expand.grid(id=(10^4*params$pi.s+1):10^4,x=c(0,1:x_M_2)), 
            data.frame(x=c(0,1:x_M_2), EV.curr=0, 
                       a=c(rep(0,x_M_2+1),rep(1,x_M_2+1)),
                       beta=params$beta,
                       x_M_2=x_M_2) %>% 
              mutate(util=ifelse(a==0,
                                 params$theta[1]*params$s.val[2]-params$theta[2]*x,
                                 0)) %>% 
              valueIteration(.)) %>% 
    mutate(a=rbernoulli(n(),prob)*1) %>% arrange(id,x) %>% 
    mutate(rowx=ifelse(a==1,x,x_M_2)) %>% 
    group_by(id) %>% 
    top_n(-(min(rowx)+1),x) %>%
    ungroup() %>% 
    select(id,x,a)) %T>% 
      saveRDS('../variables/sim_data.rds') %>% 
        filter(a==1) %>% with(hist(x, breaks=50))
}