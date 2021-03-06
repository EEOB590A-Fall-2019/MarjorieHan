# Linear Model practice exercise

#We are going to work with a dataset on plant traits. We will test whether leaf thickness differs between tree species and island. Today, we will only do data exploration and model building/selection. We will assess model fit and interpret model results next week. 

#Helpful scripts to have open while you work on this include: DataExplorationDay2_practice_answers.R, DataExplorationDay2.R, and LinearModels.R (from class Tuesday)

#Response: thickness (leaf thickness)
#Predictors: species, island
#Random effects: none for right now, but could add some later

#Load libraries (you'll need the tidyverse)
library(tidyverse)
#Load dataset (tidyREUtraits.csv) and name it "traits". 
traits <- read.csv("C:/Users/Marjorie/Downloads/Marjorie Final Project/Documents/tidyREUtraits.csv")
### Part 1: explore the dataset  #######

#1. Look at structure of dataset. 
str(traits)
#2. Subset to rows that have a value for leaf thickness. How many rows did you lose? 
traits <- traits %>%
  filter(!is.na(thickness))
#Also we will subset to the species that were collected across all three islands. I'll give you the code for this below. 
traits <- traits %>%
  filter(species == "aglaia"| species == "aidia" | species == "guamia" | species == "cynometra" | species == "neisosperma" | species == "ochrosia" | species == "premna")  

## Explore single variables ##
#3. Start with continuous variables - of which we only have the response (thickness)
# a) Check for outliers
ggplot(traits, aes(thickness))+
  geom_histogram()

# b) Check for zero-inflation (not relevant bc it's a measurement not a count)

# c) Check for independence in the response (is each row independent?) or might there be some patterns we are not including. 

#4. Now categorical predictors. Do you have an adequate sample size? How many measurements per level of island and per level of species? 
traits %>%
  group_by(island) %>%
  count()
traits %>%
  group_by(species) %>%
  count()
## Explore relationships between variables
#5) Check for correlations between predictors, or for categorical predictors, check to see if the sampling for each species is spread across each island. This is also useful for seeing whether you have adequate samples to run an island * species interaction. Try using group_by() and count(), and then graphing it using geom_bar() and facet_grid(). 
traits %>%
  group_by(species, island) %>%
  count()

ggplot(traits, aes(island))+
  geom_bar()+
  facet_grid(.~species)

#6) Look at relationships of Y vs X’s to see if variances are similar for each X value, identify the type of relationship (linear, log, etc.)
#plot each predictor and random effect against the response
#linear
ggplot(traits, aes(island, thickness))+
  geom_dotplot()

ggplot(traits, aes(species, thickness))+
  geom_boxplot()
### Summary of data exploration ### 
#what did you find? 
#there is a greater variation with species than with island for thickness
### Linear model #### 
# Create a linear model to test whether leaf thickness varies by island, and whether that depends on the plant species. 

#Option 1: Create a full model, remove interaction if not significant, but otherwise do not simplify. 
thickvalue <- lm(thickness ~ island*species, data = traits)
anova(thickvalue)
#Option 2: Create a full model, remove any non-significant interactions to get final model. 

#Option 3: Create a full model, and all submodels, and compare using Likelihood ratio tests (anova(mod1, mod2)) to choose the best fitting model. 

#Option 4: Create a full model and all submodels and compare AIC values to choose the best fitting model


#Next week, we will assess model fit, and then interpret results. 
