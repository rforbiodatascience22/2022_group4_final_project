

# Libraries

library(tidyverse)
library(mice)
library(dplyr)
library(callmice)
library(VIM)
library(broom)
library(ggplot2)
library(cocoreg)


#Data
Data <- read.csv(file = "data/_raw/kidney_disease.csv")



# Missing values with imputation method using Mice
Mice_data <-Data
Mice_data <- Mice_data %>%
  mutate(
    red_blood_cells  = as.factor(RB_cells),
    Pus_cells = as.factor(Pus_cell),
    Pus_cell_clumps = as.factor(Pus_cell_clump),
    Bacteria  = as.factor(Bacteria),
    Hypertension = as.factor(Hypertension),
    Diabetes = as.factor(Diabetes) ,
    CAD	  = as.factor(CAD),
    Appetite = as.factor(Appetite),
    Pedal_edema	 = as.factor(Pedal_edema),
    Anemia = as.factor(Anemia),
    Disease_no = as.factor(Disease_no)
    
  )
imputed_Data <- mice(Mice_data, m=5, maxit = 50, method = 'pmm')
summary(imputed_Data)
Clean_data <- complete(imputed_Data,5)

# PCA

pca_fit <- Clean_data %>% 
  select(where(is.numeric)) %>%
  prcomp(scale = TRUE)

# Visualization

mice_plot <- aggr(Mice_data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Mice_data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

mice_plot1 <- aggr(Clean_data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Clean_data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

var_explained <- pca_fit$sdev^2/sum(pca_fit$sdev^2)
pca_fit$x %>% 
  as.data.frame %>%
ggplot(aes(x=PC1,y=PC2)) + geom_point(size=1) +
  theme_bw(base_size=19) + 
  labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
       y=paste0("PC2: ",round(var_explained[2]*100,1),"%")) +
  theme(legend.position="top")

#Lets see the difference between imputation and mean for in our pca
pca_fit2 <-my_data_clean_aug %>%  
  select(where(is.numeric)) %>%
  prcomp(scale = TRUE)

var_explained <- pca_fit2$sdev^2/sum(pca_fit2$sdev^2)
pca_fit2$x %>% 
  as.data.frame %>%
  ggplot(aes(x=PC1,y=PC2)) + geom_point(size=1) +
  theme_bw(base_size=19) + 
  labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
       y=paste0("PC2: ",round(var_explained[2]*100,1),"%")) +
  theme(legend.position="top")
