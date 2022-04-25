
# Libraries ---------------------------------------------------------------

library("tidyverse")


# Functions ---------------------------------------------------------------

source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

my_data_clean <- read_tsv(file = "data/02_my_data_clean.tsv")


# Wrangle data ------------------------------------------------------------

#You should think of the augment script as the place, 
#where you create your database for everything that happens afterwards
#f.ex if we add a new variable subsch as BMI

my_data_clean_aug <- my_data_clean %>%
  mutate(my_data_clean ,
         disease_no = case_when(dm == "no" & cad == "no" & pe == "no" ~ 0 ,
                                dm == "yes" & cad == "no" & pe =="no" ~ 1 ,
                                dm == "no" & cad == "yes" & pe == "no" ~ 1 ,
                                dm == "no" & cad == "no" & pe == "yes" ~ 1 ,
                                dm == "yes" & cad == "yes" & pe == "no" ~ 2 ,
                                dm == "no" & cad == "yes" & pe == "yes" ~ 2 ,
                                dm == "yes" & cad == "no" & pe == "yes" ~ 2 ,
                                dm == "yes" & cad == "yes" & pe =="yes" ~ 3) ,
         disease_type = case_when(disease_no == 1 & dm == "yes" ~ "dm" ,
                                  disease_no == 1 & cad == "yes" ~ "cad" ,
                                  disease_no == 1 & pe == "yes" ~ "pe")
         )

#add age groups variable

my_data_clean_aug <- my_data_clean_aug %>%
  mutate(my_data_clean ,
         age_group = case_when(age <= 20 ~ "0-20" ,
                               20 < age & age <= 40 ~ "20-40" ,
                               40 < age & age <= 60 ~ "40-60" ,
                               60 < age & age <= 80 ~ "60-80" ,
                               80 < age ~ ">80")
         )

# Write data --------------------------------------------------------------

write_tsv(x = my_data_clean_aug,
          file = "data/03_my_data_clean_aug.tsv")
