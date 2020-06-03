options(error=utils::recover)



library('tidyverse')
c('reshape2', 'stringr', 'magrittr', 'doParallel') %>%
  walk(~library(., character.only=TRUE))

dir('modules') %>% 
  walk(~source(paste('./modules/', ., sep="")))

var_save <- '../variables/'

registerDoParallel(cores=28)