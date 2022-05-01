# Define project functions ------------------------------------------------

normalize_cf_matrix = function(fited_model) {
  cf_mat_normalized = fited_model %>% 
    collect_predictions() %>% 
    conf_mat(Class, .pred_class) %>% 
    tidy() %>% 
    mutate(Truth = c('ckd','ckd','notckd','notckd'),
           Prediction = c('ckd','notckd','ckd','notckd'),
           Values = c(round(value[1] / (value[1] + value[2]), 2),
                      round(value[2] / (value[1] + value[2]), 2),
                      round(value[3] / (value[3] + value[4]), 2),
                      round(value[4] / (value[3] + value[4]), 2))) %>% 
    select(!c(name,value))
  return(cf_mat_normalized)
}


plot_cf_matrix = function (cf_mat) {
  confusion_matrix = cf_mat %>% 
    ggplot(aes(x = Truth,
               y = Prediction,
               fill = Values)) +
    geom_tile() +
    geom_text(aes(label = Values), 
              color = "white", 
              size = 4) +
    scale_fill_viridis(alpha = 0.85) +
    xlab('Truth') +
    ylab('Prediction') + 
    labs(title = "Confusion matrix") +
    theme(axis.text.x = element_text(angle = 45,
                                     vjust = 1,
                                     hjust = 1,
                                     size = 12),
          axis.text.y = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "none")
  plot(confusion_matrix)
  return(confusion_matrix)
  }
