# Libraries ---------------------------------------------------------------
install.packages("reshape")
install.packages("viridis")
install.packages("hrbrthemes")
library(tidyverse)
library(viridis)
library(reshape)
library(tidyverse)
library(hrbrthemes)

# Functions ---------------------------------------------------------------

source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

my_data = read_csv(file = "data/01_my_data.csv")

## Plot missing values ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

missing_values = my_data %>% 
        summarize_all(funs(sum(is.na(.)) * 100 / length(.))) %>% 
        pivot_longer(!id, 
                     names_to="columns", 
                     values_to="missing_data") %>% 
        select(columns, 
               missing_data) %>% 
        arrange(desc(missing_data)) %>% 
        ggplot(aes(x = reorder(columns,
                               desc(missing_data)), 
                   y = missing_data)) +
        geom_col() +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45,
                                         vjust = 1, 
                                         hjust = 1)) +
        xlab('Attributes') +
        ylab('Missing data')


## Casting numbers treaed as chars to dtpýpe numeric----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

my_data_clean = my_data %>% 
  mutate(pcv = as.numeric(pcv) ,
         wc = as.numeric(wc) ,
         rc = as.numeric(rc))


## Replacing numeric missing values to median of each of the columns----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
my_data_clean = my_data_clean %>% 
  mutate_if(is.numeric , 
            function(x) ifelse(is.na(x) , 
                               median(x , 
                                      na.rm = T) , 
                               x))


## Dropping missing values in categorical columns----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

my_data_clean = my_data_clean %>% 
  na.omit()

## Fixing misspelled target variable and cad column----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

my_data_clean = my_data_clean %>% 
                mutate(classification = ifelse(classification == "ckd\t",  "ckd", classification),
                cad = ifelse(cad == "\tno","no",cad))

## Casting chars to categorical variable ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Having categorical variables we can easily cast them to numerical right before plotting or modelling. Casting formula below.
# mutate_if(is.factor, as.numeric)
# Casting was not applied here not to loose the meaning of each of the vars. Encoded labels will be less explenatory at this point.

my_data_clean = my_data_clean %>%
  mutate_if(is.character, as.factor)

#Rename Variables
my_data_clean <- as_tibble(my_data_clean) %>%
  dplyr::rename(ID = id) %>%
  dplyr::rename(Age = age) %>%
  dplyr::rename(Blood_pressure = bp) %>%
  dplyr::rename(Specific_gravity = sg) %>%
  dplyr::rename(Albumin = al) %>%
  dplyr::rename(Sugar = su) %>%
  dplyr::rename(RB_cells = rbc) %>%
  dplyr::rename(Pus_cell = pc) %>%
  dplyr::rename(Pus_cell_clump = pcc) %>%
  dplyr::rename(Bacteria = ba) %>%
  dplyr::rename(Blood_glucose = bgr) %>%
  dplyr::rename(Blood_urea = bu) %>%
  dplyr::rename(Serum_creatinine = sc) %>%
  dplyr::rename(Sodium = sod) %>%
  dplyr::rename(Potassium = pot) %>%
  dplyr::rename(Hemoglobin = hemo) %>%
  dplyr::rename(Packed_cell_vol = pcv) %>%
  dplyr::rename(WB_count = wc) %>%
  dplyr::rename(RB_count = rc) %>%
  dplyr::rename(Hypertension = htn) %>%
  dplyr::rename(Diabetes = dm) %>%
  dplyr::rename(CAD = cad) %>%
  dplyr::rename(Appetite = appet) %>%
  dplyr::rename(Pedal_edema = pe) %>%
  dplyr::rename(Anemia = ane) %>%
  dplyr::rename(Class = classification) 




## Write data ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")

## Write plots ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggsave("missing_values.png", path = "figures" , plot = missing_values)



