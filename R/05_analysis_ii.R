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

my_data_clean_aug_mod = my_data_clean_aug %>%
  select(!c(ID, Disease_type, 
            Age_group)) %>% 
  mutate_if(is.character, 
            as.factor)

data_split = initial_split(my_data_clean_aug_mod, 
                           prop = 0.7, 
                           strata = Class)
training_data = training(data_split)
testing_data = testing(data_split)

kfold = vfold_cv(training_data, 
                 v = 10)

mdl_recipe= training_data %>% 
  recipe(Class ~.) %>% # Model Class(y) as the function of all other calls
  step_range(all_numeric(), 
             -all_outcomes()) %>% #Each column will have mean of 0
  step_dummy(all_nominal(), 
             -all_outcomes(), 
             one_hot = TRUE) %>% # One hot encoding all categorical attributes
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
  select(-.estimator) %>% 
  add_row(lin_reg1_fit %>%
            select(.predictions) %>%
            unnest(cols = .predictions) %>%
            roc_auc(Class, 
                    .pred_ckd) %>%
            select(.estimate) %>% 
            mutate(.metric = 'auc')
          ) %>% 
  mutate(mdl = 'lm - all cols')

### lin_reg1_fit ROC - in house func for plotting

roc_lin_reg1 = metrics_lin_reg1 %>%
  select(-mdl) %>% 
  plot_roc('ROC for linear regression - all predictors') +
  theme_grey(base_size = 13) 
plot(roc_lin_reg1)

### Plotting normalized confusion matrix

conf_mat_lin_reg_1 = lin_reg1_fit %>%
  collect_predictions() %>% 
  conf_mat(Class, 
           .pred_class) %>% 
  normalize_cf_matrix() %>%
  plot_cf_matrix(title = "Linear regression - all predictors",
                 subtitle = "Labelling: ckd = sick with chronic kidney disease, notckd = healthy")
plot(conf_mat_lin_reg_1)

## Linear Regression v2 using two attributes---------------------------------------------

training_data = training(data_split) %>% 
  select(Packed_cell_vol, 
         Hemoglobin, 
         Class)

testing_data = testing(data_split) %>% 
  select(Packed_cell_vol, 
         Hemoglobin, Class)

mdl_2_recipe = training_data %>% 
  recipe(Class ~.) %>% # Model Class(y) as the function of all other calls
  step_range(all_numeric(), 
             -all_outcomes()) %>% #Each column will have mean of 0
  step_dummy(all_nominal(), 
             -all_outcomes(), 
             one_hot = TRUE) %>% # One hot encoding all categorical attributes
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

lin_reg2_fit = fit(lin_reg2,
                   training_data)

# Metrics 

metrics_lin_reg2 = lin_reg2_fit %>%
  predict(testing_data %>% 
            select(-Class)) %>% 
  bind_cols(testing_data) %>%
  conf_mat(Class,
           .pred_class) %>%
  summary() %>% 
  select(!.estimator) %>% 
  add_row(predict(lin_reg2_fit, 
                  testing_data %>% 
                    select(-Class),
                  type ='prob') %>% 
          bind_cols(testing_data) %>%
          roc_auc(Class, 
                  .pred_ckd) %>%
          select(.estimate) %>% 
          mutate(.metric = 'auc')
  ) %>% 
  mutate(mdl = 'lm - hemo and pcv')

### lin_reg2_fit ROC CURVE

roc_lin_reg2 = lin_reg2_fit %>%
  predict(testing_data %>% 
          select(-Class)) %>% 
  bind_cols(testing_data) %>% 
  conf_mat(Class, 
           .pred_class) %>% 
  summary() %>%
  select(-.estimator) %>% 
  plot_roc('ROC for linear regression - hemoglobin and packed cell volume')
plot(roc_lin_reg2)

### Plotting normalized confusion matrix

conf_mat_lin_reg_2 = lin_reg2_fit %>%
  predict(testing_data %>% 
            select(-Class)) %>% 
  bind_cols(testing_data) %>%
  conf_mat(Class,
           .pred_class) %>%
  normalize_cf_matrix() %>%
  plot_cf_matrix(title = "Linear regression - hemoglobin and packed cell volume", 
                 subtitle = "Labelling: ckd = sick with chronic kidney disease, notckd = healthy") 

plot(conf_mat_lin_reg_2)

## K- nearest_neighbors------------------------------------------------------------------

kfold = vfold_cv(training_data, 
                 v = 10)

mdl_3_recipe = training_data %>% 
  recipe(Class ~.) %>% # Model Class(y) as the function of all other calls
  step_range(all_numeric(), 
             -all_outcomes()) %>% #Each column will have mean of 0
  step_dummy(all_nominal(), 
             -all_outcomes(), 
             one_hot = TRUE) %>% # One hot encoding all categorical attributes
  #step_pca(all_predictors(), num_comp=30) %>% 
  prep()

knn_clf = nearest_neighbor(neighbors = tune()) %>% 
  set_mode('classification') %>% 
  set_engine('kknn')

## Tunning parameters

model_control = control_grid(save_pred = TRUE)

knn_clf_grid = grid_regular(neighbors(),
                            levels = 10)
knn_clf_tune = tune_grid(knn_clf,
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

knn_final_res = last_fit(knn_final_model, 
                         data_split)

#KNN ROC

roc_knn = knn_final_res %>% 
  collect_predictions() %>% 
  conf_mat(Class, 
           .pred_class) %>% 
  summary() %>% 
  select(-.estimator) %>% 
  plot_roc('ROC K-Nearest Neighbors - hemoglobin and PCV')
plot(roc_knn)


### Plotting normalized confusion matrix

conf_mat_knn = knn_final_res %>% 
  collect_predictions() %>% 
  conf_mat(Class, 
           .pred_class) %>% 
  normalize_cf_matrix() %>% 
  plot_cf_matrix(title = 'K-nearest neighbors - hemoglobin and PCV',
                 subtitle = "Labelling: ckd = sick with chronic kidney disease, notckd = healthy")
plot(conf_mat_knn)

# Classification metrics tibble

metrics_knn = knn_final_res %>% 
  collect_predictions() %>%
  conf_mat(Class, 
           .pred_class) %>% 
  summary() %>% 
  select(!.estimator) %>% 
  add_row(knn_final_res %>% 
            collect_predictions() %>% 
            roc_auc(Class, .pred_ckd) %>%
            select(.estimate) %>% 
            mutate(.metric = 'auc')
  ) %>% 
  mutate(mdl = 'knn')

all_metrics = rbind(metrics_lin_reg1, 
      metrics_lin_reg2) %>% 
  rbind(metrics_knn)


metrics_summary =  all_metrics %>% filter(.metric %in% c('auc',
                      'sens',
                      'spec',
                      'accuracy')) %>% 
  ggplot(aes( x = reorder(mdl, 
                          .estimate),
              y = .estimate,
              group = 1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~.metric,
             scales = "free_y") +
  xlab('Models') +
  ylab('Estimate test score') + 
  ggtitle('Performance of different training models') +
  theme(legend.position = "none") +
  theme_grey(base_size = 13)
plot(metrics_summary)


roc_all = all_metrics %>% 
  filter(.metric %in% c('sens',
                        'spec')) %>% 
  pivot_wider(values_from = .estimate,
              names_from = .metric) %>% 
  rbind(
    tibble(sens = c(0.001,
                  1.001,
                  0.001,
                  1.001,
                  0.001,
                  1.001), 
         spec = c(1.001,
                  0.001,
                  1.001,
                  0.001,
                  1.001,
                  0.001),
         mdl = c('lm - all cols',
                'lm - all cols',
                'lm - hemo and pcv',
                'lm - hemo and pcv',
                'knn',
                'knn')
         )
  ) %>% 
  ggplot(aes(x = 1-spec,
             y = sens)
  ) + 
  geom_line(aes(linetype = mdl)) +
  scale_fill_viridis(discrete = TRUE) +
  xlab('1 - Specificity') +
  ylab('Sensitivity') +
  ggtitle('Combined ROC of the trained models') +
  labs(linetype = 'Models') +
  theme_grey(base_size = 13)

plot(roc_all)
  
# Saving plots--------------------------------------------------------------

ggsave("05_lin_reg1_confusion_matrix_normalized.png",
       path = "doc/images/figures" , 
       plot = conf_mat_lin_reg_1, 
       width = 6, 
       height = 6)
ggsave("05_lin_reg2_confusion_matrix_normalized.png",
       path = "doc/images/figures", 
       plot = conf_mat_lin_reg_2,
       width = 6, 
       height = 6)
ggsave("05_knn_confusion_matrix_normalized.png",
       path = "doc/images/figures",
       plot = conf_mat_knn,
       width = 6,
       height = 6)
ggsave("05_knn_clf_tuning_fold.png",
       path = "doc/images/figures",
       plot = knn_metrics_fold,
       width = 6,
       height = 4)
ggsave("05_knn_clf_tuning_mean.png",
       path = "doc/images/figures",
       plot = knn_tuning,
       width = 6,
       height = 4)
ggsave("05_roc_knn.png",
       path = "doc/images/figures",
       plot = roc_knn,
       width = 6,
       height = 4)
ggsave("05_roc_lin_reg1.png",
       path = "doc/images/figures",
       plot = roc_lin_reg1,
       width = 6,
       height = 4)
ggsave("05_roc_lin_reg2.png",
       path = "doc/images/figures",
       plot = roc_lin_reg2,
       width = 6,
       height = 4)
ggsave("05_roc_all.png",
       path = "doc/images/figures",
       plot = roc_all,
       width = 6,
       height = 4)
ggsave("05_metrics_summary.png",
       path = "doc/images/figures",
       plot = metrics_summary,
       width = 6,
       height = 4)

