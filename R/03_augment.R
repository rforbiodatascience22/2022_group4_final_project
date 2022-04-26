
# Libraries ---------------------------------------------------------------

library("tidyverse")


# Functions ---------------------------------------------------------------

source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------



# Wrangle data ------------------------------------------------------------

#You should think of the augment script as the place, 
#where you create your database for everything that happens afterwards
#f.ex if we add a new variable subsch as BMI

my_data_clean_aug <- my_data_clean %>%
  mutate(my_data_clean ,
         Disease_no = case_when(Diabetes == "no" & CAD == "no" & Pedal_edema == "no" ~ 0 ,
                                Diabetes == "yes" & CAD == "no" & Pedal_edema =="no" ~ 1 ,
                                Diabetes == "no" & CAD == "yes" & Pedal_edema == "no" ~ 1 ,
                                Diabetes == "no" & CAD == "no" & Pedal_edema == "yes" ~ 1 ,
                                Diabetes == "yes" & CAD == "yes" & Pedal_edema == "no" ~ 2 ,
                                Diabetes == "no" & CAD == "yes" & Pedal_edema == "yes" ~ 2 ,
                                Diabetes == "yes" & CAD == "no" & Pedal_edema == "yes" ~ 2 ,
                                Diabetes == "yes" & CAD == "yes" & Pedal_edema =="yes" ~ 3) ,
         Disease_type = case_when(Disease_no == 1 & Diabetes == "yes" ~ "dm" ,
                                  Disease_no == 1 & CAD == "yes" ~ "cad" ,
                                  Disease_no == 1 & Pedal_edema == "yes" ~ "pe")
         )

#add Age groups variable

my_data_clean_aug <- my_data_clean_aug %>%
  mutate(my_data_clean ,
         Age_group = case_when(Age <= 20 ~ "0-20" ,
                               20 < Age & Age <= 40 ~ "20-40" ,
                               40 < Age & Age <= 60 ~ "40-60" ,
                               60 < Age & Age <= 80 ~ "60-80" ,
                               80 < Age ~ ">80")
         )

# Write data --------------------------------------------------------------

write_tsv(x = my_data_clean_aug,
          file = "data/03_my_data_clean_aug.tsv")
