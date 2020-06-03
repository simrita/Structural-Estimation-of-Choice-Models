#############Notes:
#plot_sim_data
  #Need brackets to pipe into ggplot
#############

plot_sim_data <- 
  . %>% {
    ggplot(data = .) +
      aes(
        x = x, 
        y = n/sum(n), 
        colour = as.factor(s),
      ) +
      geom_line() +
      labs(
        x = 'Mileage at Replacement',
        y = 'Density'
      ) +
      theme(
        legend.title = element_text(size=0),
        panel.background = element_rect(fill='white', colour='black')
      )  
  } %>% 
  ggsave(
    filename = '../plot/hist.png',
    width = 8.5
  )
  
