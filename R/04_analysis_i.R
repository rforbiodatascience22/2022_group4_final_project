# Load libraries ----------------------------------------------------------

library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------

#any extra transformation before modelling
my_data_clean_aug


# Model data

#logistic regression, PCA, clustering using k-means etc
my_data_clean_aug %>% ...


# Visualize data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------

write_tsv(...)
ggsave(...)