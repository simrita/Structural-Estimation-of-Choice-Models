list.of.packages <- c("fExtremes", "tidyverse","modelr","aod")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(fExtremes)
library(tidyverse)
library(modelr)
library(aod)
values <- readRDS("values.rds")
buckets=unique(values$bucket)
choices=unique(values$choice)
set.seed(2020)
n.sim = 10000

# simulate epsilon's and eta's
errors <- rgev(n=(n.sim*(length(buckets)+length(buckets)*length(choices))), mu=digamma(1),beta=1,xi=0)
errors <- matrix(errors, nrow=n.sim, ncol=length(buckets)+length(buckets)*length(choices))

# extract lambda
lambda = unique(values$lambda)

# compute LSE(uj/lambda_j) for each bucket
LSE <- values %>% group_by(bucket) %>% mutate(LSE = log(sum(exp(val/lambda)))) %>% 
  ungroup() %>% select(bucket,LSE) %>% unique()

# reorder values data frame by bucket
values <- values %>% arrange(bucket)

# compute uij/lambdaj
u_lambda = values$val/values$lambda
## We create a numeric id for each bucket and choice combination
# create bucket*choice id (1 to 9)
id <- values %>% select(bucket,choice) %>% mutate(id = c(1:(length(buckets)*length(choices))))

# choice simulation 1 For two step process- First we select the bucket, next we select the choice within that bucket
choice.bucket = apply(errors[,1:length(buckets)],1,function(x) which.max((x)+lambda*LSE$LSE))
errors_bucket = cbind(errors,choice.bucket)
### This function returns the optimal choice for a given bucket
choice = function(vec){
  error = vec[1:(length(buckets)+length(buckets)*length(choices))]
  error2 = vec[(length(buckets)+1):(length(buckets)+length(buckets)*length(choices))]
  choice_bucket = vec[(length(buckets)+length(buckets)*length(choices))+1]
  val2=cbind(values,error=error2)
  set=val2[val2$bucket==choice_bucket,]
  choice_opt=set$choice[which.max(set$val+set$lambda*set$error)]
  return(choice_opt)
}
choice.item=apply(errors_bucket,1,function(x) choice(x))
choice_1 <-  data.frame(bucket = choice.bucket, choice = choice.item)
choice_1 <-  data.frame(bucket = choice.bucket, choice = choice.item) %>% 
  arrange(bucket, choice)
choice_1 <- merge(choice_1,id)
# choice_1_id<-apply(choice_1,1,function(x) id$id[which(id$bucket==x[1]&id$choice==x[2])])
# choice_1 <- cbind(choice_1,id=choice_1_id)

# choice simulation 2.. one shot choice
choice = function(vec) {
 err_b=cbind(bucket=buckets,error_bucket=vec[1:length(buckets)])
 err_id=data.frame(id=id$id,error_id=vec[(length(buckets)+1):(length(buckets)+length(buckets)*length(choices))])
 set1=merge(values,err_b)%>% arrange(bucket, choice)
 set2=merge(set1,id)%>% arrange(bucket, choice)
 set3=cbind(set2,err_id$error_id)
 return(which.max(set3$error_bucket+set3$val+set3$lambda*set3$`err_id$error_id`))
}
choice_2 = data.frame(id=apply(errors,1,choice))
choice_2 = merge(choice_2,id)
# estimate choice probabilities
choice_prob_hat_1 = choice_1 %>% group_by(id) %>% mutate(prob = n()/n.sim) %>% 
                             unique() %>% ungroup() %>% select(prob) %>% unlist()
choice_prob_hat_2 = choice_2 %>% group_by(id) %>% mutate(prob = n()/n.sim) %>% 
                             unique() %>% ungroup() %>% select(prob) %>% unlist()

# theoretical choice probabilities
values = merge(values, LSE)
values <- values %>% group_by(bucket) %>% mutate(denom1 = sum(exp(val/lambda))) %>% ungroup()
values <- values %>% group_by(choice) %>% mutate(denom2 = sum(exp(lambda*LSE))) %>% ungroup()
values <- values %>% mutate(choice_prob_theo = (exp(lambda*LSE))*exp(val/lambda)/(denom1*denom2))                                                
choice_prob_theo = as.vector(values$choice_prob_theo)


# bootstrapping choices to obtain covariance matrices

n.boot = 1000
choice.prob.hat.1 = matrix(NA,nrow=length(id$id),ncol=n.boot)
choice.prob.hat.2 = matrix(NA,nrow=length(id$id),ncol=n.boot)

for(i in 1:n.boot) {
  choice.prob.hat.1[,i] = data.frame(resample_bootstrap(choice_1)) %>% group_by(id) %>% 
    mutate(prob = n()/n.sim) %>% unique() %>% ungroup() %>% select(prob) %>% unlist()
  choice.prob.hat.2[,i] = data.frame(resample_bootstrap(choice_2)) %>% group_by(id) %>% 
    mutate(prob = n()/n.sim) %>% unique() %>% ungroup() %>% select(prob) %>% unlist()
}
mean_choice.prob.hat.1 = apply(choice.prob.hat.1,1,mean)
mean_choice.prob.hat.2 = apply(choice.prob.hat.2,1,mean)

cov_1 = cov(t(choice.prob.hat.1))
cov_2 = cov(t(choice.prob.hat.2))

# wald test

wald.test(Sigma = cov_1[1:(length(id$id)-1),1:(length(id$id)-1)], b = choice_prob_hat_1[1:(length(id$id)-1)], Terms=1:(length(id$id)-1), 
          H0 = choice_prob_theo[1:(length(id$id)-1)])

wald.test(Sigma = cov_2[1:(length(id$id)-1),1:(length(id$id)-1)], b = choice_prob_hat_2[1:(length(id$id)-1)], Terms=1:(length(id$id)-1), 
          H0 = choice_prob_theo[1:(length(id$id)-1)])

# print probabilities
round(cbind(choice_prob_theo,choice_prob_hat_1,choice_prob_hat_2),3)
