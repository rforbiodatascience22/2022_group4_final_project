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

modelling = fit_predict_visualize(lin_reg1, data_split)

