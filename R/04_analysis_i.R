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

#all preparation for plotting

my_data_clean_aug_age = my_data_clean_aug %>% 
  select(age, age_group, classification) 


my_data_clean_aug_disease = my_data_clean_aug %>% 
  select(id, dm, cad, pe, classification, disease_no) %>%
  filter(classification == "ckd")

  
my_data_clean_aug_which_disease = my_data_clean_aug %>%
  select(id, dm, cad, pe, classification, disease_no, disease_type) %>%
  filter(disease_no == 1,
         classification == "ckd") 

my_data_clean_aug_infection = my_data_clean_aug %>%
  select(id , age, ba, wc, classification) %>%
  filter(classification == "ckd") 

my_data_clean_aug_wc = my_data_clean_aug %>%
  select(id , age, ba, wc, classification) 

my_data_clean_aug_diabetes = my_data_clean_aug %>%
  select(id ,
         dm ,
         sg ,
         su ,
         bgr , 
         classification) %>%
  filter(classification == "ckd")

my_data_clean_aug_redblood = my_data_clean_aug %>%
  select(id ,
         rbc ,
         hemo , 
         rc ,
         ane , 
         classification)

my_data_clean_aug_cor = my_data_clean_aug %>% 
  select(-classification, everything()) %>% 
  select(!disease_type) %>% 
  mutate_if(is.factor, as.numeric) %>%  # Converting factors to numerical on the spot
  print() %>% 
  cor(use = 'all.obs', method = 'pearson') %>%
  round(2) %>%
  melt()


# Visualize data ----------------------------------------------------------

## Correlation heatmap

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
              

##age distribution plot

age_distribution = ggplot(my_data_clean_aug_age , 
                           mapping = aes(x = age_group,
       fill = classification))+
geom_bar(binwidth = 2)+
labs(x = "Age",
     y = "count",
     title = "Distribution of patients' age groupped by CKD and no CKD"
)+
scale_fill_manual(values=c("#69b3a2", "#404080"))
  plot(age_distribution)

##disease presence besides ckd
disease_no = ggplot(my_data_clean_aug_disease , 
                     mapping = aes(x = disease_no
                     ))+
  geom_histogram()+
  labs(x = "Number of diseases",
       y = "Frequency",
       title = "Number of extra diseases for patients with CKD"
  )
  plot(disease_no)
  

##which disease is most associated
  
find_disease = ggplot(my_data_clean_aug_which_disease , 
                       mapping = aes(x = disease_type))+
  geom_bar() +
  labs(x = "Disease type",
       y = "Count",
       title = "Number of CKD patients with one other disease"
  )
plot(find_disease)

## find correlation between wc and infection status

find_infection = ggplot(my_data_clean_aug_infection , 
                         mapping = aes(x = id , 
                                       y = wc ,
                                       colour = ba))+
  geom_bar(stat = "identity", width=0.2)+
  labs(x = "Patient ID",
       y = "White blood cell count",
       title = "White blood cell count for each CKD patient, grouped by bacterial presence"
  ) +
  coord_flip()
plot(find_infection)

##distribution of white blood cells count

wc = ggplot(my_data_clean_aug_wc , 
                         mapping = aes(x = wc , 
                                       fill = classification))+
  geom_histogram()+
  labs(x = "White blood cells count",
       y = "Frequency",
       title = "Distribution of white blood cell count for CKD and non-CKD patients"
  )
plot(wc)

##investigate diabetes markers - correlation with sugar levels

diabetes = ggplot(my_data_clean_aug_diabetes , 
                  mapping = aes(x = bgr , 
                                group = dm ,
                                fill = dm))+
  geom_density(adjust=1.5) +
  facet_wrap(~dm) +
  labs(x = "Blood glucose level (mgs/dl)",
       y = "Density",
       title = "Blood glucose for CKD patients with/without diabetes"
  )
plot(diabetes)

##investigate red blood cell count in ckd

red_blood = ggplot(my_data_clean_aug_redblood , 
                  mapping = aes(x = hemo , 
                                y = rc ,
                                alpha = ane))+
  geom_point(size=2, color="red") +
  facet_wrap(~classification) +
  labs(x = "Hemoglobin level (gms)",
       y = "Red blood cell count (millions/cm)",
       title = "Relationship between hemoglobin and red blood cells count" ,
       subtitle = "in patients with/without anemia and with/without CKD"
  )
plot(red_blood)


#plot relationship between hemoglobin and albumin grouped by classification

hemo_al_plot = my_data_clean %>% 
  select(classification, hemo, al) %>% 
  ggplot(mapping = aes(x = al , 
                       y = hemo ,
                       fill = classification))+
  geom_boxplot()

plot(hemo_al_plot)


#plot relationship between hemoglobin and packed cell volume grouped by classification
#A convention that has been adopted in medicine is to estimate haemoglobin (HB)
#concentration as a third of packed cell volume (PCV) or vice versa. 


hemo_pvc_plot = my_data_clean %>% 
  select(classification, hemo, pcv) %>% 
  ggplot(mapping = aes(x = pcv , 
                       y = hemo ,
                       color = classification))+
  geom_point() +
  theme_minimal()+
  labs(x = "Packed cell volume",
       y = "Hemoglobin (gms)",
       title = "Relationship between hemoglobin and packed cell volume"
  )

plot(hemo_pvc_plot)

# Write data --------------------------------------------------------------
#write_tsv(...)

ggsave("hemo_pcv_relationship.png", path = "figures" , plot = hemo_pvc_plot, width = 8, height = 5)
ggsave("cor_heatmap.png", path = "figures" , plot = cor_heatmap, width = 8, height = 5)
ggsave("age_distribution.png", path = "figures/distributions" , plot = age_distribution)
ggsave("disease_no_association.png", path = "figures/" , plot = disease_no)
ggsave ("which_disease.png" , path = "figures/distributions" , plot = find_disease)
ggsave ("infection.png" , path = "figures/distributions" , plot = find_infection)
ggsave ("WC.png" , path = "figures/distributions" , plot = wc)
ggsave ("diabetes_glucose_level.png" , path = "figures/distributions" , plot = diabetes)
ggsave ("redblood.png" , path = "figures" , plot = red_blood)

