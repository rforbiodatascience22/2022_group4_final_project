
# Libraries ---------------------------------------------------------------

library("tidyverse")


# Functions ---------------------------------------------------------------

source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

my_data <- read_csv(file = "data/01_my_data.csv")


# Wrangle data ------------------------------------------------------------

#here comes the cleaning script

my_data_clean <- my_data


# Write data --------------------------------------------------------------

write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")
