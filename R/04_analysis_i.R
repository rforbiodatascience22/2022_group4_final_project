# Load libraries ----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(reshape)
library(viridis)
library(hrbrthemes)
library(forcats)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug = read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------

#any extra transformation before modelling
my_data_clean_aug = my_data_clean_aug %>% 
  mutate_if(is.character, 
            as.factor)

#Creating variables for plotting

my_data_clean_aug_disease = my_data_clean_aug %>% 
  select(ID, 
         Diabetes, 
         CAD, 
         Pedal_edema, 
         Class, 
         Disease_no) %>%
  filter(Class == "ckd")


my_data_clean_aug_infection = my_data_clean_aug %>%
  select(ID, 
         Age, 
         Bacteria, 
         WB_count, 
         Class) %>%
  filter(Class == "ckd") 


my_data_clean_aug_diabetes = my_data_clean_aug %>%
  select(ID,
         Diabetes,
         Specific_gravity,
         Sugar,
         Blood_glucose , 
         Class) %>%
  filter(Class == "ckd")

#Data frame for data cleaning plot
NAs_progress = data.frame(step = c("Raw data", 
                                   "Replacing NAs (numerical)",
                                   "Dropping NAs (categorical)"), 
                          size = c(400.001, 
                                   400, 
                                   233), 
                          variable_name = c("my_data", 
                                            "my_data_clean", 
                                            "my_data_clean"), 
                          percent = c(1,
                                      1,
                                      0.58))

NAs_progress = as.tibble(NAs_progress)

# Visualize data ----------------------------------------------------------

#Cleaning progress

cleaning_plot = NAs_progress  %>% 
  mutate(step = fct_reorder(step, 
                            size)) %>%
  ggplot(aes(x = step, 
             y = size, 
             label = scales::percent(percent))) +
  geom_bar(fill = '#482677ff', 
           stat="identity", 
           alpha=0.6, 
           width=0.4) +
  coord_flip() +
  geom_text(nudge_y = -50,
            color = "white",
            size = 5,
            fontface = "bold") +
  labs(x = "",
       y = "Number of observations in the dataset",
       title = "Data cleaning process") +
       theme_grey(base_size = 13) 

plot(cleaning_plot)

## Correlation heatmap
cor_heatmap = my_data_clean_aug %>% 
  select(!Disease_no) %>%
  select(!Age_group) %>%
  select(-Class,
         everything()) %>% 
  select(!Disease_type) %>% 
  mutate_if(is.factor,
            as.numeric) %>%  
  print() %>%
  cor(use = 'all.obs', 
      method = 'pearson') %>%
  round(2) %>%
  melt() %>%
  ggplot(aes(x = X1,
             y = X2,
             fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), 
                color = "white", 
                size = 1.7) +
  theme_bw()+
  scale_fill_viridis(alpha = 0.8) +
  xlab('') +
  ylab('') + 
  labs(title = "Correlation of the attributes with the target variable", 
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

##Age distribution plot

age_distribution =  my_data_clean_aug %>% 
  select(Age,
         Age_group, 
         Class) %>% 
  ggplot(mapping = aes(x = Age_group,
                       fill = Class)) +
  geom_bar(bindwidth = 2, alpha = 0.6) +
  labs(x = "Age group",
      y = "count",
      title = "Distribution of age grouppes for patients with/without CKD") +
  scale_fill_manual(values = c('#482677ff',
                               '#36bb75ff')) +
  theme_grey()
  
  

plot(age_distribution)

##disease presence besides ckd

disease_no = ggplot(my_data_clean_aug_disease, 
                    mapping = aes(x = Disease_no)) +
  geom_histogram() +
  labs(x = "Number of diseases",
       y = "Frequency",
       title = "Number of extra diseases for patients with CKD")

plot(disease_no)
  

##which disease is most associated
  
find_disease =  my_data_clean_aug %>%
  select(ID, 
         Diabetes, 
         CAD, 
         Pedal_edema, 
         Class, 
         Disease_no, 
         Disease_type) %>%
  filter(Disease_no == 1,
         Class == "ckd") %>%
  ggplot(mapping = aes(x = Disease_type)) +
  geom_bar() +
  labs(x = "Disease type",
       y = "Count",
       title = "Number of CKD patients with one other disease")


plot(find_disease)

## find correlation between wc and infection status

find_infection = ggplot(my_data_clean_aug_infection,
                        mapping = aes(x = ID, 
                                      y = WB_count ,
                                      colour = Bacteria)) +
  geom_bar(stat = "identity", 
           width = 0.2) +
  labs(x = "Patient ID",
       y = "White blood cell count",
       title = "White blood cell count for each CKD patient, grouped by bacterial presence") +
  coord_flip()

plot(find_infection)

##distribution of white blood cells count

wc =  my_data_clean_aug %>%
  select(ID,
         Age,
         Bacteria,
         WB_count,
         Class) %>% 
  ggplot(mapping = aes(x = WB_count, 
                           fill = Class)) +
  geom_histogram() +
  labs(x = "White blood cells count",
       y = "Frequency",
       title = "Distribution of white blood cell count for CKD and non-CKD patients")

plot(wc)

##investigate diabetes markers - correlation with sugar levels

diabetes = ggplot(my_data_clean_aug_diabetes,
                  mapping = aes(x = Blood_glucose, 
                                group = Diabetes ,
                                fill = Diabetes)) +
  geom_density(adjust = 1.5) +
  facet_wrap(~ Diabetes) + 
  labs(x = "Blood glucose level (mgs/dl)",
       y = "Density",
       title = "Blood glucose for CKD patients with/without diabetes")

plot(diabetes)

##investigate red blood cell count in ckd

red_blood = my_data_clean_aug %>%
  select(ID,
         RB_cells,
         Hemoglobin, 
         RB_count,
         Anemia, 
         Class) %>% 
  ggplot(mapping = aes(x = Hemoglobin, 
                       y = RB_count,
                       color = Anemia)) +
  geom_point(size = 2.5, 
             alpha = 0.6) +
  scale_color_manual(values = c('#482677ff',
                                '#36bb75ff')) +
  facet_wrap(~ Class) +
  labs(x = "Hemoglobin level (gms)",
       y = "Red blood cell count (millions/cm)",
       title = "Relationship between hemoglobin and red blood cells count",
       subtitle = "in patients with/without anemia and with/without CKD") +
  theme_grey(base_size = 13)


plot(red_blood)


#plot relationship between hemoglobin and albumin grouped by classification

hemo_al_plot = my_data_clean %>% 
  select(Class,
         Hemoglobin,
         Albumin) %>% 
  ggplot(mapping = aes(x = Albumin, 
                       y = Hemoglobin,
                       fill = Class)) +
  scale_fill_manual(values = c('#482677ff',
                               '#36bb75ff')) +
                      geom_boxplot(alpha = 0.6) +
  labs(x = "Albumin",
       y = "Hemoglobin (gms)",
       title = "Relationship between hemoglobin and albumin", 
       subtitle = "Labelling: ckd = sick with chronic kidney disease, notckd = healthy",
       fill = "Patient status") +
  theme_grey(base_size = 13)


plot(hemo_al_plot)

#relationship between hemoglobin and packed cell volume grouped by classification

hemo_pvc_plot = my_data_clean %>%
  select(Class, 
         Hemoglobin,
         Packed_cell_vol) %>% 
  ggplot(mapping = aes(x = Packed_cell_vol, 
                       y = Hemoglobin,                                     color = Class))+
                       geom_point(size = 2.5, 
                                  alpha = 0.6) +
  scale_color_manual(values = c("#482677ff",
                                "#36bb75ff")) +
  labs(x = "Packed cell volume",
       y = "Hemoglobin (gms)",
       title = "Relationship between hemoglobin and packed cell volume",
       subtitle = "Labelling: ckd = sick with chronic kidney disease, notckd = healthy",
       color = "Patient status")+
  theme_grey(base_size = 13)

plot(hemo_pvc_plot)

lm_fit <- lm(my_data_clean ~ Serum_creatinine + Blood_urea , data=df)
summary(lm_fit)

#Serum creatinine and blood urea relationship
serum_urea_plot = my_data_clean %>% 
  select(Class, 
         Blood_urea, 
         Serum_creatinine) %>% 
  ggplot(mapping = aes(x = Serum_creatinine, 
                       y = Blood_urea))+
  geom_point(aes(color = Class), 
             size = 2.5, 
             alpha = 0.6 ) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("#482677ff",
                                "#36bb75ff")) +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10')+
  labs(x = "Serum creatinine (mgs/dl)",
       y = "Blood urea (mgs/dl)",
       title = "Relationship between serum creatinine and blood urea",
       subtitle = "Labelling: ckd = sick with chronic kidney disease, notckd = healthy",
       color = "Patient status") +
  theme_grey(base_size = 13) 


plot(serum_urea_plot)


# Write data --------------------------------------------------------------
#write_tsv(...)
ggsave("cleaning_plot.png", 
       path = "figures" , 
       plot = cleaning_plot, 
       width = 8, 
       height = 5)

ggsave("creatinine_urea_relationship.png", 
       path = "figures" , 
       plot = serum_urea_plot, 
       width = 8, 
       height = 5)

ggsave("hemo_pcv_relationship.png", 
       path = "figures" , 
       plot = hemo_pvc_plot, 
       width = 8, 
       height = 5)

ggsave("cor_heatmap.png", 
       path = "figures" , 
       plot = cor_heatmap, 
       width = 8, 
       height = 5)

ggsave("hemo_albumin_relat.png", 
       path = "figures" , 
       plot = hemo_al_plot, 
       width = 8, 
       height = 5)

ggsave("age_distribution.png", 
       path = "figures/distributions" , 
       plot = age_distribution, 
       width = 8, 
       height = 5)

ggsave("disease_no_association.png",
       path = "figures/" , 
       plot = disease_no, 
       width = 8, 
       height = 5)

ggsave ("which_disease.png" , 
        path = "figures/distributions" ,
        plot = find_disease, 
        width = 8, 
        height = 5)

ggsave ("infection.png" ,
        path = "figures/distributions" , 
        plot = find_infection, 
        width = 8, 
        height = 5)

ggsave ("WC.png" , 
        path = "figures/distributions" , 
        plot = wc, 
        width = 8, 
        height = 5)

ggsave ("diabetes_glucose_level.png" , 
        path = "figures/distributions" , 
        plot = diabetes, 
        width = 8, 
        height = 5)

ggsave ("redblood.png" , 
        path = "figures" ,
        plot = red_blood,
        width = 8, 
        height = 5)

