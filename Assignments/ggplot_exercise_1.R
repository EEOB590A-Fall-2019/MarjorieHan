#ggplot exercise Day 1
#EEOB590

#We will use the forest trajectory dataset to make some graphs. These are from 25m transects conducted across three islands within 4 different forest types. We measured a bunch of things along each transect, so the dataframe is relatively complex. Be sure to use the ggplot_tutorial.R script to help you with this exercise. 

#Load libraries
library(tidyverse)
library(ggplot2)
library(ggthemes)

#Load data
foresttraj <- read.csv("C:/Users/Marjorie/Downloads/Marjorie Final Project/Documents/foresttrajectory_site_tidy.csv")
#1) Replicate the figure in the graphics folder called spprich.adult.pdf. 
ggplot(foresttraj, aes(forest.type, num.adults, fill=factor(island)))+
  geom_boxplot()


#2) Now, make a figure based on model output from the model below. The final figure should look like the one called num.adult.trees.pdf. Be sure to use the code in the ggplot_tutoria file for this. 

m1 <- glm(num.adults ~ island, data=foresttraj, family=poisson)
ggplot(m1, aes(island, num.adults))+
  geom_boxplot()
#3) Come up with a cool way to visualize the relationship between the number of adult species and the number of seedling species across the islands and forest types. 
ggplot(foresttraj, aes(num.adults, num.saplings))+
  geom_boxplot()+
  facet_grid(.~island)

#4) Find a cool graphical approach from the websites below, then create a graph of that type using data from the foresttraj dataset 
# http://www.r-graph-gallery.com/ 
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html 
ggplot(foresttraj, aes(num.saplings, num.adults, fill=factor(island)))+
  geom_hex()

