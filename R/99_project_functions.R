# Define project functions ------------------------------------------------

fit_predict_visualize = function(mdl_worflow,data_split) {
  
  mdl_fit = last_fit(mdl_worflow,
                     data_split)
  
  metrics_mdl_fit = mdl_fit %>% 
    collect_predictions() %>% 
    conf_mat(Class, 
             .pred_class) %>% 
    summary() %>% print()
  
  
  ### mdl_fit ROC CURVE
  
  mdl_fit %>% 
    select(.predictions) %>% 
    unnest(cols = .predictions) %>%
    roc_curve(Class, .pred_ckd) %>% autoplot()
  
  ### AUC values
  AUC_mdl_fit = mdl_fit %>% 
    select(.predictions) %>% 
    unnest(cols = .predictions) %>%
    roc_auc(Class, .pred_ckd) %>%
    select(.estimate) %>% 
    .[[1]] %>% 
    round(3)

  return(tibble(mdl = mdl_fit, 
                metrics = metrics_mdl_fit,
                AUC = AUC_mdl_fit))
  }


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


plot_cf_matrix = function (cf_mat, title) {
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
    labs(title = title) +
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
