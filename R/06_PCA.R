

# Libraries

library(tidyverse)
library(mice)
library(dplyr)
library(callmice)
library(VIM)
library(broom)
library(ggplot2)
library(cocoreg)
library(reshape2)
library(ggfortify)


#Data
Data <- read.csv(file = "data/_raw/kidney_disease.csv")


# Missing values with imputation method using Mice
Mice_data <-Data
Mice_data <- Mice_data %>%
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.factor, as.numeric)  
imputed_Data <- mice(Mice_data, m=5, maxit = 50, method = 'pmm')
summary(imputed_Data)
Clean_data <- complete(imputed_Data,5)

# PCA

pca_fit <- Clean_data %>% 
  prcomp(scale= TRUE)

pca_fit2 <-my_data_clean_aug %>%  
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

PCAvalues <- data.frame(class =Clean_data$classification , pca_fit$x)
PCAloadings <- data.frame(Variables = rownames(pca_fit$rotation), pca_fit$rotation)
var_explained <- pca_fit$sdev^2/sum(pca_fit$sdev^2)


ggplot(PCAvalues, aes(x = PC1, y = PC2, colour = Clean_data$classification)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*15),
                                       yend = (PC2*15)), arrow = arrow(length = unit(1/2, "picas")),
               color = "black") +
  geom_point(size = 1) +
  annotate("text", colour= "red",fontface =3, x = (PCAloadings$PC1*17), y = (PCAloadings$PC2*17),
           label = PCAloadings$Variables)+
  labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
       y=paste0("PC2: ",round(var_explained[2]*100,1),"%"),
       legend.text = element_text("classification"))
       

#Lets see the difference between imputation and mean for our pca

var_explained <- pca_fit2$sdev^2/sum(pca_fit2$sdev^2)

pca_fit2$x %>% 
  as.data.frame %>%
  ggplot(aes(x=PC1,y=PC2)) + geom_point(size=1) +
  theme_bw(base_size=19) + 
  labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
       y=paste0("PC2: ",round(var_explained[2]*100,1),"%")) +
  theme(legend.position="top")


#Fortify_plots

autoplot(pca_fit, data = Clean_data, colour = Clean_data$classification ,labels= TRUE)
autoplot(pca_fit, data = Clean_data, colour = Clean_data$classification, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 5,loadings.colour = 'blue',)
autoplot(pca_fit, data = Clean_data, colour =  Clean_data$classification,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
autoplot(fanny(Clean_data[-5], 3), frame = TRUE)
# variance

var_explained_df <- data.frame(pca_fit= paste0("PC",1:26),
                               var_explained=(pca_fit$sdev)^2/sum((pca_fit$sdev)^2))
var_explained_df %>%
  ggplot(aes(x=pca_fit,y=var_explained , group=1))+
  geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot: PCA on scaled data")


