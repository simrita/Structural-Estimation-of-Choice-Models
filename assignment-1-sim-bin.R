
list.of.packages <- c("optimization", "lhs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(optimization)
library(lhs)
find_best_location <- function(num.sites, assemblers, competetors, union.rate.fn, beta, num.tries){
  list_loc=list()
  list_val=vector()
  ###### Random Latin HyperCube Sampling of Starting Points
  starts=t(randomLHS(2*num.sites,num.tries))
for(try in 1:num.tries){
  cost.fn=function(locations){
  locations=matrix(locations,ncol=2,nrow =num.sites)
  values=vector()
  for (r in 1:nrow(assemblers)){
    exp_cost1=0
    for(i in 1:nrow(locations)){
     exp_cost1=exp_cost1+exp(-beta$distance*dist(rbind(locations[i,],assemblers[r,]))-beta$union*union.rate.fn(locations[i,1],locations[i,2]))
    }
    exp_cost2=0
    for(j in 1:nrow(competetors)){
      exp_cost2=exp_cost2+exp(-beta$distance*dist(rbind(competetors[j,],assemblers[r,]))-beta$union*union.rate.fn(competetors[j,1],competetors[j,2])) 
    }
  values[r]=log(exp_cost1+exp_cost2)
  }
  return(-sum(values))
}
optimal=optim(par = as.numeric(starts[try,]), fn=cost.fn, control = list(maxit=10000))
list_loc[[try]]=optimal$par
list_val[try]=optimal$value
}
mini=which.min(list_val)
return(matrix(list_loc[[mini]],ncol=2,nrow =num.sites))
}
locations=find_best_location(num.sites=6, assemblers=assembly, competetors=competetor, union.rate.fn=unionizationRate, beta=costParameters, num.tries=30)
print(locations)
saveRDS(locations,'solution_sim_bin.rds')

