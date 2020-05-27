library('tidyverse')
c('reshape2', 'magrittr') %>%
  walk(~library(., character.only=TRUE))

dir('modules') %>% 
  walk(~source(paste('./modules/', ., sep="")))

set.seed(2020)