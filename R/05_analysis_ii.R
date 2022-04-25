# Inspired by https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
# https://www.youtube.com/watch?v=am3dgqjd19U&ab_channel=AndrewCouch

# Load libraries ----------------------------------------------------------

library(tidymodels)
library(yardstick) #it is part of tidymodels but needed to be loaded to solve namespaces problem
library(tidyverse)
library(viridis)
library(vip)
library(kknn)
library(reshape)
set.seed(9966) #Control randomness

# Define functions --------------------------------------------------------

source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------

my_data_clean_aug = read_tsv(file = "data/03_my_data_clean_aug.tsv")

# Preprocessing ------------------------------------------------------------
my_data_clean_aug = my_data_clean_aug %>%
  select(!disease_type) %>% 
  mutate_if(is.character, as.factor)



data_split = initial_split(my_data_clean_aug, prop = 0.7, strata = classification)
training_data = training(data_split)
testing_data = testing(data_split)

kfold = vfold_cv(training_data, v = 10)

mdl_recipe = training(data_split) %>% 
                recipe(classification ~.) %>% # Model classification(y) as the func of all other calls
                step_rm(id) %>% # id should not be random it will be deleted
                step_range(all_numeric(), -all_outcomes()) %>% #Each column will have mean of 0
                step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% # One hot encoding all categorical attributes
                prep()

# Metrics ------------------------------------------------------------


c_metrics = metric_set(accuracy,
                       sens,
                       yardstick::spec,
                       roc_auc)        

# Modelling ------------------------------------------------------------

knn_clf = nearest_neighbor(neighbors = tune()) %>% 
  set_mode('classification') %>% 
  set_engine('kknn')

model_control = control_grid(save_pred = TRUE)

knn_clf_grid = grid_regular(neighbors(),
                           levels = 10)
knn_clf_tune = tune_grid(
  knn_clf,
  mdl_recipe,
  resamples = kfold,
  control = model_control,
  metrics = c_metrics)

# Estimators visualization ------------------------------------------------------------


knn_metrics = knn_clf_tune %>% 
              collect_metrics() %>% 
              ggplot(aes( x = neighbors,
                          y = mean)) +
              geom_point() +
              geom_line() +
              facet_wrap(~.metric,
                         scales = "free_y") +
              xlab('Neighbors number') +
              ylab('Mean') + 
              ggtitle('Performance of K-Nearest Neighbors classifier') +
              theme_minimal()
plot(knn_metrics)

knn_metrics_fold = knn_clf_tune %>%
                   select(id, .metrics) %>% 
                   unnest(.metrics) %>% 
                   ggplot(aes( x = neighbors,
                               y = .estimate,
                               color = id)) +
                   geom_point() +
                   geom_line() +
                   facet_wrap(~.metric,
                              scales = "free_y") +
                   xlab('Neighbors number') +
                   ylab('Estimate') + 
                   ggtitle('Performance of K-Nearest Neighbors classifier') +
                   theme_minimal() +
                   theme(legend.position = "none")
plot(knn_metrics_fold)

confusion_matrix = knn_clf_tune %>% 
    collect_predictions() %>% 
    conf_mat(classification, .pred_class) %>% 
    print() %>% 
    tidy() %>% 
    ggplot(aes(x = Truth,
               y = Prediction,
               fill = n)) +
    geom_tile() +
    geom_text(aes(label = n), 
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

knn_clf_tune %>% 
  collect_predictions() %>% 
  conf_mat(classification, .pred_class) %>% 
  summary() %>% 
  select(-detection_prevalence) %>% 
  ggplot(aes(x = reorder(.metric,
                         .estimate),
             y = .estimate)) +
  geom_col() +
  geom_hline(yintercept = 0.95,
             linetype="dashed", 
             color = "red",
             size = 1.5) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  xlab('') +
  ylab('Estimator value') +
  ggtitle('KNN classifier test performance') +
  coord_flip()
  
#   ggplot(aes(x = name,
#              y = name,
#              fill = value)) +
#   geom_tile() +
#   geom_text(aes(label = value), 
#             color = "white", 
#             size = 1.7) +
#   scale_fill_viridis(alpha = 0.85) +
#   xlab('') +
#   ylab('') + 
#   labs(title = "Confusion matrix") +
#   theme(axis.text.x = element_text(angle = 45,
#                                    vjust = 1,
#                                    hjust = 1),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank())
# plot(cor_heatmap)

  
# rf_clf = rand_forest(trees = 100) %>%
#   set_mode('classification') %>%
#   set_engine("randomForest")
# 
# model_rf %>%
#   predict(test) %>%
#   bind_cols(test) %>%
#   metrics(truth = classification, estimate = .pred_class)
# 
# rf_proba = model_rf %>%
#             predict(test) %>%
#             bind_cols(test, type = "proba") %>%
#             glimpse()
# 
# rf_proba%>%
#   gain_curve(classification, .pred_class) %>%
#   glimpse()
# 
# glimpse(model_rf)


#Feature importance extraction
#https://www.tidymodels.org/start/case-study/

# Visualize data ----------------------------------------------------------

my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
ggsave("class_distribution.png", path = "figures" , plot = y_distr, width = 4, height = 3)
ggsave("knn_clf_metrics.png", path = "figures" , plot = knn_metrics, width = 6, height = 4)
ggsave("knn_clf_metrics_fold.png", path = "figures" , plot = knn_metrics_fold, width = 6, height = 4)
ggsave("knn_confusion_matrix.png", path = "figures" , plot = confusion_matrix, width = 6, height = 6)



write_tsv(...)
ggsave(...)