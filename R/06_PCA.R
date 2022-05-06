

# Libraries

library(tidyverse)
library(dplyr)
library(tidymodels)
library(ggplot2)


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")

# Wrangle data ------------------------------------------------------------

#any extra transformation before modelling
my_data_clean_aug1 <- my_data_clean_aug %>% 
  mutate_if(is.character, 
            as.factor) %>% 
  mutate_if(is.factor, 
            as.numeric) %>% 
  subset( select = -c(Disease_type))


# PCA--------------------------------------------------------------------------------------


pca_rec <- recipe(~., 
                  data = my_data_clean_aug1) %>%
  step_center(all_numeric()) %>%
  # center the data
  step_scale(all_numeric()) %>%
  # pca on all numeric variables
  step_pca(all_numeric())

pca_prep <- prep(pca_rec)

pca_rec2 <- recipe(~., 
                   data = my_data_clean_aug1) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep2 <- prep(pca_rec2)

#all preparation for plotting----------------------------------------------------------------

sdev <- pca_prep$steps[[2]]$res$sdev
percent_variation <- sdev^2 / sum(sdev^2)
var_df <- data.frame(PC = paste0("PC",
                                 1:length(sdev)),
                     var_explained = percent_variation,
                     stringsAsFactors = FALSE)

tidied_pca <- tidy(pca_prep2, 2)


# Visualization ----------------------------------------------------------------------------
var_df %>%
  mutate(PC = fct_inorder(PC)) %>%
  ggplot(aes(x = PC,y=var_explained)) + 
  geom_col() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


pca_plot = juice(pca_prep) %>%
  ggplot(aes(PC1, 
             PC2)) +
  geom_point(aes(colour = pull(my_data_clean_aug,
                               Class)), 
             alpha = 0.6, 
             size = 2.5) +
  scale_color_manual(values = c('#482677ff',
                               '#36bb75ff')) +
  labs(title = "PCA",
       colour = "Class", 
       subtitle = "Labelling: ckd = sick with chronic kidney disease, notckd = healthy") +
  theme_grey(base_size = 13)

plot(pca_plot)
  
  
tidied_pca %>%
    filter(component %in% paste0("PC", 1:5)) %>%
    mutate(component = fct_inorder(component)) %>%
    ggplot(aes(value, 
               terms, 
               fill = terms)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ component, 
               nrow = 1) +
    labs(y = NULL)  
  
tidied_pca %>%
    filter(component %in% paste0("PC", 6:10)) %>%
    mutate(component = fct_inorder(component)) %>%
    ggplot(aes(value, 
               terms, 
               fill = terms)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~component, 
               nrow = 1) +
    labs(y = NULL)  
  
tidied_pca %>%
    filter(component %in% paste0("PC", 11:15)) %>%
    mutate(component = fct_inorder(component)) %>%
    ggplot(aes(value, 
               terms, 
               fill = terms)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ component, 
               nrow = 1) +
    labs(y = NULL)  
  
tidied_pca %>%
    filter(component %in% paste0("PC", 16:20)) %>%
    mutate(component = fct_inorder(component)) %>%
    ggplot(aes(value, 
               terms, 
               fill = terms)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ component, 
               nrow = 1) +
    labs(y = NULL) 
  
tidied_pca %>%
    filter(component %in% paste0("PC", 21:26)) %>%
    mutate(component = fct_inorder(component)) %>%
    ggplot(aes(value, 
               terms, 
               fill = terms)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ component, 
               nrow = 1) +
    labs(y = NULL)  
  
tidied_pca %>%
    filter(component %in% paste0("PC", 1:2)) %>%
    mutate(component = fct_inorder(component)) %>%
    ggplot(aes(value, 
               terms, 
               color = terms)) +
    geom_point(size = 2.5) +
    labs(y = NULL) 

#------Writing Data------------------------------------------------------------------------
ggsave ("pca_plot.png" , 
        path = "figures" ,
        plot = pca_plot,
        width = 8, 
        height = 5)

                
                
                

  
  