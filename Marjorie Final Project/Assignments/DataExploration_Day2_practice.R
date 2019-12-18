#Data Exploration and visualization practice exercise
#Haldre Rogers
#EEOB590

#Research Question: 
#does survival of seedlings depend on distance from nearest conspecific adult

library(ggplot2)
library(skimr)
library(tidyverse)
library(DataExplorer)

######### Data preparation ##################

#1) start with a tidy dataset
fencesurv <- read.csv("data/tidy/fencesurv_tidy.csv", header=T, na.strings=c("", "NA", "na")) 
fences <- fencesurv_tidy

#############################################
#data dictionary
# "species"- six plant species     
# "disp" - disperser present on island - yes/no          
# "island" - island (guam, saipan, tinian, rota)     
# "site"    - 5 sites on Guam, 3 each on Rota, Tinian, Saipan         
# "fence"   - fence name (based on forest plot grid names)       
# "numalive"  - number seedlings alive in fence 
# "date"       - date fence checked     
# "observer"   - person collecting data      
# "dataentry"   - person entering data     
# "dateenter"    - date data entered    
# "uniqueidsppfence" - unique id for each spp:fence combo
# "canopydate"    - date canopy cover data taken 
# "north"          - canopy measurement 1  
# "east"           - canopy measurement 2     
# "south"            - canopy measurement 3  
# "west"             - canopy measurement 4   
# "avgcover"        -average canopy measurement (% cover)    
# "avgopen"          -average canopy measurement (% open)   
# "doubleplant"     - was this fence double planted? 
# "plantdt"          - planting data
# "dist"             - near or far from conspecific? 
# "soil"             - soil type within the fence
# "numseedplant"    - number of seedlings planted
# "DDsurvival_notes"  - notes
# "bird"             - bird presence or absence on the island
# "age"             - age of seedlings (since planting)
# "centavgopen"      - centered average open
# "adultdens_wdisp"  - adult tree density on islands with disperser for that spp
# "adultdens_wodisp" - adult tree density on islands without disperser for that spp
# "seedsize"       - seed size 
# "numtrees"        - number of conspecific trees in the plot 
# "area"            - area of the plot
# "dens_100m"       - calculated density per 100 m
# "regdens"         - density across all plots
# "regdenswd"       - density just from plots with dispersers for that species
# 
#############################################

#2) check structure to make sure everything is in correct class
str(fences)
summary(fences)
#3) Subset to the dataset you will use for the analysis
#we will use all of the dataset, but if we were to subset, we would use filter. 
fencesurvkarst <-fencesurv %>%
  filter(soil=="karst")

nrow(fencesurvkarst)#88 observations

# We will make a new column for propalive
fences <- fences %>%
  mutate(propalive = numalive/numseedplant)

#4) Decide which variables are your response variables and which are your predictors
# Response: cbind(numalive, numseedplant) or propalive
# Continuous predictors: distance, centavgopen
# Categorical predictors: species, substrate, open, parent (?) 
# Random effects: island (n=4 usually), site (n=3/island)

############ Data Exploration ##########
#5) try these two functions, from the skimr and DataExplorer packages
skim(fences)
create_report(fences)

##########
#5a) Look for outliers in continuous response & predictor variables. 
# Use histogram or dotplot, identify outliers

ggplot(fences, aes(propalive))+
  geom_histogram()

ggplot(fences, aes(centavgopen))+
  geom_histogram()

dotchart(fences$propalive) #no outliers
dotchart(fences$centavgopen) #two outliers, both at far distances and both on Guam - maybe should remove?

##########
#5b) Zero-inflation in y variable (for count data)? 

sum(fences$numalive/fences$numseedplant== 0) / length(fences$numalive/fences$numseedplant) #17% zeros

papaya<-fences %>%
  filter (species=="papaya")

sum(papaya$numalive/papaya$numseedplant== 0) / length(papaya$numalive/papaya$numseedplant) #57% zeros. 

with(papaya[papaya$numalive>0,], table(island, dist)) #some alive at each distance on each island, except Saipan far (why is this??)  # check raw data

##########
# 6a) Collinearity X: correlation between covariates ()
#Plot each predictor against each other (since categorical, can't test this directly)
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
#Plot response against each predictor and random effect. 
#As long as at least one is continuous, use ggplot (use facet_grid for multiple categorical variables)
#Use summarize or table for two categorical (ftable for more than 2 categories)


MyVar <- c("species", "island", "dist", 
           "centavgopen", "site", "soil")
pairs(fences[,MyVar], lower.panel = panel.cor)

###########################
####Look for missing data and relationships between variables #######
#
##check out soil data
mytable<-with(fences, table (species, island, dist, soil)) 
mytable<- xtabs(~species+island+dist+soil, data=fencesurv)
ftable(mytable) #not a lot of papaya data for substrate. Lots of substrate/dist/spp/island combos with zero, one or two samples. 

#openness relative to distance
ggplot(fences, aes(dist, centavgopen))+
  geom_boxplot() #two outliers for openness are at far distances, maybe remove?

ggplot(fences, aes(soil, centavgopen))+
  geom_boxplot()+
  ylim(0,40) #centavgopen is similar for all soil types

ggplot(fences, aes(species, centavgopen))+
  geom_boxplot()+
  ylim(0,40) #some variation in centavgopen between spp, but not too much. 

ggplot(fences, aes(island, centavgopen))+
  geom_boxplot()+
  ylim(0,40) #guam and tinian similar, rota, saipan slightly lower. Guam has more outliers at upper openness levels. 

ggplot(fences, aes(site, centavgopen))+
  geom_boxplot()+
  #  ylim(0,40) +
  facet_grid(.~island) #lot of heterogeneity, but nothing seems consistent between islands- just site to site differences. #openness data missing for tinian. 

##########
#plot response against each potential predictor and random effect- linear relationships? 
ggplot(fences, aes(dist, numalive/numseedplant))+
  geom_boxplot()+
  facet_grid(soil~species) #some soil data missing for morinda, papaya, aglaia(?)

ggplot(fences, aes(dist, numalive/numseedplant))+
  geom_boxplot()+
  facet_grid(.~species) #many papaya are dead.Few aglaia, neiso, psychotria are dead.  
ggplot(fences[fences$centavgopen<35,], aes(centavgopen, numalive/numseedplant))+
  geom_point()+
  facet_grid(soil~species) #very few canopy open data points beyond 30. Could consider truncating at 35 to remove influential points
#potentially nonlinear (saturating) relationship between openness and prop survival

ggplot(fences, aes(centavgopen, numalive/numseedplant))+
  geom_point()+
  facet_grid(dist~species) 

ggplot(fences, aes(island, numalive/numseedplant))+
  geom_boxplot()+
  facet_grid(dist~species) #lot of island differences

ggplot(fences, aes(site, numalive/numseedplant))+
  geom_boxplot()+
  facet_grid(dist~species) #lot of site differences

############
# Interactions- do we have enough data? 
with(fences, ftable(species, dist, island))

numsamp<- fences %>%
  group_by(island, species, soil) %>%
  summarize(count=length(soil))

ggplot(numsamp, aes(soil, count))+
  geom_boxplot()+
  facet_grid(species~island)+
  theme_bw() #some uneven samples - e.g. lots mix on Guam, whereas more soil on Rota and Saipan. 

#check to see which spp we are missing soil or openness data for
fencesNA <- fences %>%
  filter(is.na(centavgopen) | is.na(soil)) 

summary(fencesNA) #mostly papaya, some morinda; lots on tinian missing openness data

############
#Summary of data exploration

#1: Outliers
#No outliers in response. Some NA's in response- will need to clean this up. 
#Some outliers in canopy (high values)- both are far and on Guam. Should remove from analysis. 
#distance, species and substrate are all categorical. 

#2: Zero-inflation
# 17% zeros. this is distributed unevenly across spp, though, with 56% zeros for papaya.Keep an eye on papaya- low sample size. 

#3: Collinearity: No strong collinearities. Heterogeneity, though. 

#4: Linearity & homogeneity- relationship of Y vs X's. 
# potential nonlinear relationship between centavgopen and propsurvival (saturating?) but maybe the link function will take care of this? 
# heterogeneity in a variety of response factors. 

#5: Independence Y- 
# random effects of site and island warranted. Note- 4 levels of island, 3-5 levels of site within island so many would question this as a random effect because not enough levels. Also, not all spp planted on all islands. 
# temporal correlation: all samples taken at same time, but could look at effect of age of seedlings on prop survival
# I don't think there would be spatial correlation beyond site, but could examine response against latitude. 

#6: Interactions - do we have enough data? 
#Low numbers of papaya, and then have to remove lots bc no soil data. 
#low numbers for some soil:species:island interactions. 
#No neiso planted on Rota, no Psychotria planted on Tinian
#
#decided not to analyze soil relationships because we are missing soil data for a significant number of plants. 

#############################################
#remove two extreme datapoints with really high canopy open values because they are only on Guam and at far distances - have undue influence
fences<-fences[is.na(fences$centavgopen) | fences$centavgopen<35,] #lost two points (both guam and far)

#########################################
# Make dataset without NA's in avgopen
fencesnoNA<-subset(fences, !is.na(centavgopen)) #dataset without na's in open
dim(fencesnoNA) #lost fences by removing data with na's in open -mostly japc data
