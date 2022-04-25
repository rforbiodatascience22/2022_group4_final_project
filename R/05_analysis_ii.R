# Load libraries ----------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(vip)
#Control randomness
set.seed(9966)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

my_data_clean_aug = read_tsv(file = "data/03_my_data_clean_aug.tsv")
my_data_clean_aug = my_data_clean_aug %>%
  select(!disease_type) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.factor, as.numeric) %>% 
  mutate(classification = as.factor(classification))

# Wrangle data ------------------------------------------------------------
# Inspired by https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
<<<<<<< HEAD
# https://www.youtube.com/watch?v=am3dgqjd19U&ab_channel=AndrewCouch

=======
>>>>>>> 661efe1daa0af8bb151f78aaaa0360157b372ac3

my_data_clean_aug %>% 
  group_by(classification) %>% 
  count()

#There are two special interpretations of . in a formula. 
#The usual one is in the context of a data argument of model fitting functions and means
#‘all columns not otherwise in the formula’: see terms.formula. In the context of
#update.formula, only, it means ‘what was previously in this part of the formula’.
data_split = initial_split(my_data_clean_aug, prop = 0.7)

# Preprocessing------------------------------------------------------------

#Transforming training data and preparing receipe for testing data
recipe = training(data_split) %>% 
  recipe(classification ~.) %>% #model classification(y) as the func of all other calls
  #step_corr(all_predictors()) %>% #step corr removes variables of high correlations with each othe (dimension reduction)
  step_center(all_predictors(), -all_outcomes()) %>% #minus means selecting all X no y (target variable)
  step_scale(all_predictors(), -all_outcomes()) %>% #minus means selecting all X no y (target variable)
  prep()
print(recipe)

test = recipe %>%
  bake(testing(data_split)) 
glimpse(test)

train = juice(recipe) #training split can be found in the receip so we are using juice function to extract it
glimpse(train)

model_rf = rand_forest(trees = 100,
                    mode = 'classification') %>% 
           set_engine("randomForest") %>% 
           fit(classification ~ ., 
               data = train)

model_rf %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = classification, estimate = .pred_class)

rf_proba = model_rf %>%
            predict(test) %>%
            bind_cols(test, type = "proba") %>% 
            glimpse()

rf_proba%>%
  gain_curve(classification, .pred_class) %>%
  glimpse()

glimpse(model_rf)

#Feature importance extraction
#https://www.tidymodels.org/start/case-study/
  
# Model data
#if we perform any kind of second analysis

my_data_clean_aug %>% ...


# Visualize data ----------------------------------------------------------

my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------

write_tsv(...)
ggsave(...)