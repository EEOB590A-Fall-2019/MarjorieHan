# 26 September 2019 ####

#data wrangling part 1, practice script
#we will be working with a real insect pan traps dataset that I've amended slightly in order to practice the skills from Tuesday. 

#1) load libraries - you will need tidyverse and readxl
library(readxl)
library(tidyverse)
#2) Read in data
#data imported
  
#3) rename columns. Leave insect families with capital letters, but make all other columns lowercase. Remove any spaces. Change "location" to "site". Change "tract" to "transect". 
names(Data_wrangling_day1_pollination2)
Data_wrangling_day1_pollination2 <- Data_wrangling_day1_pollination2 %>% 
  rename(island = Island, 
         site = Location, 
         transect = Tract,
         topbowl = 'Top color - Bowl color',
         other = Other)

colnames(Data_wrangling_day1_pollination2)
#4) Add missing data. Note that the people who entered the data did not drag down the island or location column to fill every row. 
pollination <- Data_wrangling_day1_pollination2 %>%
  fill(island, site)


#5) Separate "Top color - Bowl color" into two different columns, with the first letter for the top color and the second letter for the bowl color. We do not need to save the original column.
pollination <- pollination %>%
  separate(topbowl, into=c("topcolor", "bowlcolor"), sep="-", remove = T) 
pollination <- pollination %>%
  separate(topcolor, into=c("topcolor", "bowlcolor"), sep="-", remove = F) 

#6) Use the complete function to see if we have data for all 3 transects at each location. Do not overwrite the poll dataframe when you do this. 
pollination <- pollination %>%
  complete(site, transect)


#which transects appear to be missing, and why?
#Some of the insects are because they haven't been completed
#7) Unite island, site, transect into a single column with no spaces or punctuation between each part. Call this column uniqueID. We need to keep the original columns too. 

pollination <- pollination %>%
  unite(uniqueID, c(island, site, transect), sep="", remove=F)

#8) Now, make this "wide" dataset into a "long" dataset, with one column for the insect orders, and one column for number of insects. 
pollination_long <- pollination %>%
  gather(key = "insectorder", value = "insectnum")
#9) And just to test it out, make your "long" dataset into a "wide" one and see if anything is different. 
pollinationwide <- pollination_long %>%
  spread(key = insectorder, value = insectnum)
#are you getting an error? Can you figure out why? 
#keys are shared in the rows and it needs a unique combination
#10) Now, join the "InsectData" with the "CollectionDates" tab on the excel worksheet. You'll need to read it in, and then play around with the various types of 'mutating joins' (i.e. inner_join, left_join, right_join, full_join), to see what each one does to the final dataframe. 
dates <- Data_wrangling_day1_pollination2
pollinationdate <- pollination %>%
  inner_join(dates)
