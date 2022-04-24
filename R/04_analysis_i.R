# Load libraries ----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(reshape)
library(viridis)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug = read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------

#any extra transformation before modelling
my_data_clean_aug = my_data_clean_aug %>% 
  mutate_if(is.character, as.factor)

# Model data

#logistic regression, PCA, clustering using k-means etc
my_data_clean_aug_age = my_data_clean_aug %>% 
  select(age,classification) %>%
  mutate(age = as.numeric(age)) %>%
  drop_na()

my_data_clean_aug_disease = my_data_clean_aug %>% 
  select(id, dm, cad, pe, classification, disease_no) %>%
  filter(classification == "ckd") %>%
  drop_na()
  
my_data_clean_aug_which_disease = my_data_clean_aug %>%
  select(id, dm, cad, pe, classification, disease_no, disease_type) %>%
  filter(disease_no == 1,
         classification == "ckd") %>%
  drop_na()

my_data_clean_aug_infection = my_data_clean_aug %>%
  select(id , age, ba, wc, classification) %>%
  filter(classification == "ckd") %>%
  drop_na()

my_data_clean_aug_wc = my_data_clean_aug %>%
  mutate(wc = as.numeric(wc))%>%
  select(id , age, ba, wc, classification) %>%
  drop_na()

my_data_clean_aug_cor = my_data_clean_aug %>% 
  select(-classification, everything()) %>% 
  select(!disease_type) %>% 
  mutate_if(is.factor, as.numeric) %>%  # Converting factors to numerical on the spot
  print() %>% 
  cor(use = 'all.obs', method = 'pearson') %>%
  round(2) %>%
  melt()

# Visualize data ----------------------------------------------------------

cor_heatmap = ggplot(my_data_clean_aug_cor,
                     aes(x = X1,
                         y = X2,
                         fill = value)) +
                     geom_tile()  +
                     geom_text(aes(label = value), 
                               color = "white", 
                               size = 1.7) +
                     scale_fill_viridis(alpha = 0.85) +
                     xlab('') +
                     ylab('') + 
                     labs(title = "Correlation of the attributes and target variable", 
                          fill = "Pearson Correlation") +
                     theme(axis.text.x = element_text(angle = 45,
                                                      vjust = 1,
                                                      hjust = 1),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank())
plot(cor_heatmap)
              

age_distribution = ggplot(my_data_clean_aug_age , 
                           mapping = aes(x = age,
       fill = classification))+
geom_histogram(binwidth = 2)+
labs(x = "Age",
     y = "frequency",
     title = "Distribution of patients' age groupped by CKD and no CKD"
)+
scale_fill_manual(values=c("#69b3a2", "#404080"))
  plot(age_distribution)


disease_no = ggplot(my_data_clean_aug_disease , 
                     mapping = aes(x = disease_no
                     ))+
  geom_histogram()+
  labs(x = "Number of diseases",
       y = "Frequency",
       title = "Number of extra diseases for patients with CKD"
  )
  plot(disease_no)
  
  
find_disease = ggplot(my_data_clean_aug_which_disease , 
                       mapping = aes(x = disease_type))+
  geom_bar() +
  labs(x = "Disease type",
       y = "Count",
       title = "Number of CKD patients with one other disease"
  )
plot(find_disease)

find_infection = ggplot(my_data_clean_aug_infection , 
                         mapping = aes(x = id , 
                                       y = wc ,
                                       colour = ba))+
  geom_bar(stat = "identity", width=0.2)+
  labs(x = "count",
       y = "Patient id",
       title = "White blood cell count for each CKD patient, grouped by bacterial presence"
  )
plot(find_infection)

wc = ggplot(my_data_clean_aug_wc , 
                         mapping = aes(x = wc , 
                                       fill = classification))+
  geom_histogram()+
  labs(x = "White blood cells count",
       y = "Frequency",
       title = "Distribution of white blood cell count for CKD and non-CKD patients"
  )
plot(wc)

# Write data --------------------------------------------------------------
#write_tsv(...)

ggsave("cor_heatmap.png", path = "figures" , plot = cor_heatmap, width = 8, height = 5)
ggsave("age_distribution.png", path = "figures/distributions" , plot = age_distribution)
ggsave("disease_no_association.png", path = "figures/" , plot = disease_no)
ggsave ("which_disease.png" , path = "figures/distributions" , plot = find_disease)
ggsave ("infection.png" , path = "figures/distributions" , plot = find_infection)
ggsave ("WC.png" , path = "figures/distributions" , plot = wc)