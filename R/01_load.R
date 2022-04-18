
# Libraries ---------------------------------------------------------------

library("tidyverse")

# Functions ---------------------------------------------------------------

source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------

my_data_raw <- read_csv(file = "data/_raw/kidney_disease.csv")


# Wrangle data ------------------------------------------------------------

#this is we do the joining of meta data and the data

my_data <- my_data_raw


# Write data --------------------------------------------------------------

write_tsv(x = my_data,
          file = "data/01_my_data.tsv")

