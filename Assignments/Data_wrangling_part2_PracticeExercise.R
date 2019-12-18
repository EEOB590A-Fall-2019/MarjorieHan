# EEOB590A
# Data_wrangling part 2 practice exercise
# practice tidying and wrangling 

#from the tidy folder, read in the partialtidy file for pollination from last week's assignment
library("tidyverse")
library("lubridate")
library("readxl")
library("forcats")
###########################################################
#####Part 1: finish tidying & wrangling dataframe #########
summary(poll_long_partialtidy)
#1) Broad changes to the database
#1a) Change the class of each variable as appropriate (i.e. make things into factors or numeric)
pollweek2 <- poll_long_partialtidy %>%
  mutate_at(vars(uniqueID, island, site, transect, topcolor, bowlcolor, insectorder), 
            factor)
#1b) Change name of "date traps out" and "date traps coll" to "dateout" and "datecoll"
pollweek2 <- pollweek2 %>%
  rename(dateout = 'date.traps.out',
         datecoll = 'date.traps.coll')

#2) Fix the errors below within cells 

##2a) Fix the levels of site so that they have consistent names, all in lowercase
pollweek2$site <- as.factor(tolower(pollweek2$site)) 
  


##2b) What format are the dates in? Do they look okay? 
str(pollweek2$datecoll)
#year-month-day
##2c) Do you see any other errors that should be cleaned up? 
#I don't think so
#3) Create a new column for the duration of time traps were out
pollweek2$datecoll <- as.numeric(pollweek2$datecoll)
pollweek2$dateout <- as.numeric(pollweek2$dateout)
pollweek2 <- pollweek2 %>%
  mutate(duration = datecoll-dateout)

#4) Arrange data by the number of insects
pollweek2 <- pollweek2 %>%
  arrange(numinsects)


#5) Print tidied, wrangled database
write.csv(pollweek2)

#####################################################
####Part 3: start subsetting & summarizing ##########

#6) Make a new dataframe with just the data from Guam at the racetrack site and name accordingly. 
guampollrace <- pollweek2 %>%
  filter(island == "Guam", site == "racetrack")

#7) Make a new dataframe with just the uniqueID, island, site, transect, insectorder, numinsects, and duration columns. 
pollvariables <- pollweek2 %>%
  select(uniqueID, island, site, transect, insectorder, numinsects, duration)
#8) With the full database (not the new ones you created in the two previous steps), summarize data, to get: 

#8a) a table with the total number of insects at each site
insectpoll <- pollweek2 %>%
  group_by(site) %>%
  summarise(meaninsects = mean(numinsects, na.rm = T))
#8b) a table that shows the mean number of insects per island
insectpollisland <- pollweek2 %>%
  group_by(island) %>%
  summarise(meaninsects = mean(numinsects, na.rm = T))
#8c) a table that shows the min and max number of insects per transect
insectpolltransect <- pollweek2 %>%
  group_by(island, site, transect) %>%
  summarise(mininsects = min(numinsects, na.rm = T),
            maxinsects = max(numinsects, na.rm = T))
#9a) Figure out which insect order is found across the greatest number of sites
insectordersite <- pollweek2 %>%
  group_by(insectorder) %>%
  filter(numinsects>0) %>%
  summarise(sitenum = n_distinct(site))
  
  pollweek2 %>%
    group_by(site, insectorder) %>%
    summarise(insectinsite = sum(numinsects, na.rm = T)) %>%
    filter(insectinsite >0) %>%
    group_by(insectorder) %>%
    count(insectorder)
  
#9b) For that insect order, calculate the mean and sd by site. 
pollapo <- pollweek2 %>%
  filter(insectorder == "Apoidea") %>%
  group_by(site) %>%
  summarise(meanapo = mean(numinsects, na.rm = T),
            stdapo = sd(numinsects, na.rm = T))
