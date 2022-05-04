# Define project functions ------------------------------------------------
plot_roc = function(sens_spec_df, title){
    roc_plot = sens_spec_df %>% 
    filter(.metric %in% c('sens',
                        'spec')
           ) %>% 
    pivot_wider(values_from = .estimate,
                names_from = .metric) %>% 
    rbind(tibble(sens = c(0.001,
                          1.001), 
                 spec = c(1.001,
                          0.001))
          ) %>%
    ggplot(aes(x = 1-spec,
               y = sens)
           ) + 
    geom_line() +
    xlab('1 - Specificity') +
    ylab('Sensitivity') +
    ggtitle(title) +
    theme_grey(base_size = 13) 
    return(roc_plot)
}

normalize_cf_matrix = function(conf_mat) {
  cf_mat_normalized = 
    conf_mat %>% 
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


plot_cf_matrix = function (cf_mat, title, subtitle) {
  confusion_matrix = cf_mat %>% 
    ggplot(aes(x = Truth,
               y = Prediction,
               fill = Values)) +
    geom_tile() +
    geom_text(aes(label = Values), 
              color = "white", 
              size = 7) +
    scale_fill_viridis(alpha = 0.7) +
    xlab('Truth') +
    ylab('Prediction') + 
    labs(title = title, 
         subtitle = subtitle) +
    theme(axis.text.x = element_text(angle = 45,
                                     vjust = 1,
                                     hjust = 1,
                                     size = 12),
          axis.text.y = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "none")
  return(confusion_matrix)
}
