library('tidyverse')
c('reshape2', 'stringr', 'magrittr','chebpol', 'np') %>%
  walk(~library(., character.only=TRUE))

dir('modules') %>% 
  walk(~source(paste('./modules/', ., sep="")))

