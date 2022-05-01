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
print(paste0('Number of attributes after augmentation: ',
             my_data_clean_aug %>% 
              select(-Class) %>% 
              ncol()))

# Preprocessing ------------------------------------------------------------

my_data_clean_aug = my_data_clean_aug %>%
  select(!c(Disease_type, Age_group)) %>% 
  mutate_if(is.character, as.factor)

data_split = initial_split(my_data_clean_aug, prop = 0.7, strata = Class)
training_data = training(data_split)
testing_data = testing(data_split)

kfold = vfold_cv(training_data, v = 10)

mdl_recipe= training(data_split) %>% 
  recipe(Class ~.) %>% # Model Class(y) as the function of all other calls
  step_rm(ID) %>% # id should not be random it will be deleted
  step_range(all_numeric(), -all_outcomes()) %>% #Each column will have mean of 0
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% # One hot encoding all categorical attributes
  prep()

print(paste0('Number of attributes after dummy encoding: ',
             mdl_recipe%>% 
             juice() %>% 
             select(-Class) %>% 
             ncol()))

# Metrics ------------------------------------------------------------

c_metrics = metric_set(accuracy,
                       sens,
                       yardstick::spec,
                       roc_auc)        

# Modelling ------------------------------------------------------------
## Linear regression v1 - Using all of the attributes-------------------

lin_reg1 = workflow() %>% 
  add_model(logistic_reg() %>% 
            set_mode("classification") %>%
            set_engine("glm")) %>% 
  add_recipe(mdl_recipe)

lin_reg1_fit = last_fit(lin_reg1,
                        data_split)

metrics_lin_reg1 = lin_reg1_fit %>%
  collect_predictions() %>%
  conf_mat(Class,
           .pred_class) %>%
  summary() %>% 
  print()

### lin_reg1_fit ROC CURVE

lin_reg1_fit %>%
  select(.predictions) %>%
  unnest(cols = .predictions) %>%
  roc_curve(Class, .pred_ckd) %>% autoplot(title = "Linear regression - all attributes")

### AUC values
AUC_lin_reg1 = lin_reg1_fit %>%
  select(.predictions) %>%
  unnest(cols = .predictions) %>%
  roc_auc(Class, .pred_ckd) %>%
  select(.estimate) %>%
  .[[1]] %>%
  round(3)

print(paste0('AUC value for linear regression v1: ',AUC_lin_reg1))

### Plotting normalized confusion matrix
conf_mat_lin_reg_1 = lin_reg1_fit %>%
  normalize_cf_matrix() %>%
  plot_cf_matrix(title = "Linear regression - all attributes")
plot(conf_mat_lin_reg_1)

## Linear Regression v2 using two attributes---------------------------------------------
my_data_clean_aug_two_att = my_data_clean_aug %>% select(Packed_cell_vol, Hemoglobin, Class)
data_split = initial_split(my_data_clean_aug_two_att, prop = 0.7, strata = Class)
training_data = training(data_split)
testing_data = testing(data_split)
kfold = vfold_cv(training_data, v = 10)


mdl_2_recipe = training_data %>% 
  recipe(Class ~.) %>% # Model Class(y) as the function of all other calls
  step_range(all_numeric(), -all_outcomes()) %>% #Each column will have mean of 0
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% # One hot encoding all categorical attributes
  #step_pca(all_predictors(), num_comp=30) %>% 
  prep()

print(paste0('Number of attributes ',
             mdl_2_recipe%>% 
               juice() %>% 
               select(-Class) %>% 
               ncol()))


lin_reg2 = workflow() %>% 
  add_model(logistic_reg() %>% 
              set_mode("classification") %>%
              set_engine("glm")) %>% 
  add_recipe(mdl_2_recipe)

lin_reg2_fit = last_fit(lin_reg2,
                        data_split)

metrics_lin_reg2 = lin_reg2_fit %>%
  collect_predictions() %>%
  conf_mat(Class,
           .pred_class) %>%
  summary() %>% 
  print()

### lin_reg2_fit ROC CURVE

lin_reg2_fit %>%
  select(.predictions) %>%
  unnest(cols = .predictions) %>%
  roc_curve(Class, .pred_ckd) %>% autoplot(title = "Linear regression - hemoglobin and packed cell volume")

### AUC values
AUC_lin_reg2 = lin_reg2_fit %>%
  select(.predictions) %>%
  unnest(cols = .predictions) %>%
  roc_auc(Class, .pred_ckd) %>%
  select(.estimate) %>%
  .[[1]] %>%
  round(3)

print(paste0('AUC value for linear regression v1: ',AUC_lin_reg2))

### Plotting normalized confusion matrix
conf_mat_lin_reg_2 = lin_reg2_fit %>%
  normalize_cf_matrix() %>%
  plot_cf_matrix(title = "Linear regression - hemoglobin and packed cell volume")
plot(conf_mat_lin_reg_2)

## K- nearest_neighbors------------------------------------------------------------------
data_split = initial_split(my_data_clean_aug_two_att, prop = 0.7, strata = Class)
training_data = training(data_split)
testing_data = testing(data_split)
kfold = vfold_cv(training_data, v = 10)

mdl_3_recipe = training_data %>% 
  recipe(Class ~.) %>% # Model Class(y) as the function of all other calls
  step_range(all_numeric(), -all_outcomes()) %>% #Each column will have mean of 0
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% # One hot encoding all categorical attributes
  #step_pca(all_predictors(), num_comp=30) %>% 
  prep()

knn_clf = nearest_neighbor(neighbors = tune()) %>% 
  set_mode('classification') %>% 
  set_engine('kknn')

## Tunning parameters
model_control = control_grid(save_pred = TRUE)

knn_clf_grid = grid_regular(neighbors(),
                            levels = 10)
knn_clf_tune = tune_grid(
  knn_clf,
  mdl_3_recipe,
  resamples = kfold,
  control = model_control,
  metrics = c_metrics)

## Selecting optimal model parameters
knn_final_model = workflow() %>% 
  add_model(nearest_neighbor(neighbors =  knn_clf_tune %>% 
                               select_best(metric = "roc_auc") %>% 
                               select(neighbors) %>% 
                               .[[1]]
  ) %>%
    set_mode('classification') %>% 
    set_engine('kknn')
  ) %>% 
  add_recipe(mdl_3_recipe)

knn_final_res = last_fit(knn_final_model, 
                         data_split)

# Classification metrics tibble
knn_final_res %>% 
  collect_predictions() %>%
  conf_mat(Class, .pred_class) %>% 
  summary()

## K-nearest neighbors plotting
### Plotting metrics with different neighbors parameter
knn_tuning = knn_clf_tune %>% 
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
plot(knn_tuning)

### Plotting metrics with different neighbors parameter in each fold
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

### Confusion matrix plotting
plot_knn_cf_mat = knn_final_res %>% 
  collect_predictions() %>%
  conf_mat(Class, .pred_class) %>% 
  print() %>% 
  tidy() %>% 
  mutate(Values = n) %>% 
  select(!n) %>% 
  plot_cf_matrix('K-nearest neighbors classifier')

### Plotting normalized confusion matrix
plot_knn_cf_mat_normalized = knn_final_res %>% 
  normalize_cf_matrix() %>% 
  plot_cf_matrix('K-nearest neighbors - hemoglobin and packed cell volume')

# knn_metrics = knn_clf_tune %>% 
#   collect_predictions() %>% 
#   conf_mat(Class, .pred_class) %>% 
#   summary() %>% 
#   select(-detection_prevalence) %>% 
#   ggplot(aes(x = reorder(.metric,
#                          .estimate),
#              y = .estimate)) +
#   geom_col() +
#   geom_hline(yintercept = 0.95,
#              linetype="dashed", 
#              color = "red",
#              size = 1.5) + 
#   theme(axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12)) +
#   xlab('') +
#   ylab('Estimator value') +
#   ggtitle('KNN classifier test performance') +
#   coord_flip()

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
#   set_mode('Class') %>%
#   set_engine("randomForest")
# 
# model_rf %>%
#   predict(test) %>%
#   bind_cols(test) %>%
#   metrics(truth = Class, estimate = .pred_class)
# 
# rf_proba = model_rf %>%
#             predict(test) %>%
#             bind_cols(test, type = "proba") %>%
#             glimpse()
# 
# rf_proba%>%
#   gain_curve(Class, .pred_class) %>%
#   glimpse()
# 
# glimpse(model_rf)


#Feature importance extraction
#https://www.tidymodels.org/start/case-study/

# Visualize data ----------------------------------------------------------

#my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
#ggsave("class_distribution.png", path = "figures" , plot = y_distr, width = 4, height = 3)

ggsave("lin_reg1_confusion_matrix_normalized.png",
       path = "figures/modeling" , 
       plot = conf_mat_lin_reg_1, 
       width = 6, 
       height = 6)
ggsave("lin_reg2_confusion_matrix_normalized.png",
       path = "figures/modeling", 
       plot = conf_mat_lin_reg_2,
       width = 6, 
       height = 6)
ggsave("knn_clf_tuning_fold.png",
       path = "figures/modeling" ,
       plot = knn_metrics_fold,
       width = 6,
       height = 4)
ggsave("knn_clf_tuning_mean.png",
       path = "figures/modeling" ,
       plot = knn_tuning,
       width = 6,
       height = 4)
ggsave("knn_confusion_matrix.png",
       path = "figures/modeling", 
       plot = plot_knn_cf_mat,
       width = 6,
       height = 6)
ggsave("knn_confusion_matrix_normalized.png",
       path = "figures/modeling",
       plot = plot_knn_cf_mat_normalized,
       width = 6,
       height = 6)

write_tsv(...)
ggsave(...)