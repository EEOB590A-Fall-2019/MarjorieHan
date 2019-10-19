# 26 September 2019 ####

#data wrangling part 1, practice script
#we will be working with a real insect pan traps dataset that I've amended slightly in order to practice the skills from Tuesday. 

#1) load libraries - you will need tidyverse and readxl
library(readxl)
library(tidyverse)
#2) Read in data

transplant <- read_csv("~/R/Course-Materials/data/raw/transplant_raw.csv")
#3) rename columns. Leave insect families with capital letters, but make all other columns lowercase. Remove any spaces. Change "location" to "site". Change "tract" to "transect". 
names(transplant)
transplant <- transplant %>% 
  rename(island = Island, 
         site = Site, 
         web = Web, 
         native = Native, 
         netting = Netting, 
         startdate = Start.Date, 
         enddate = End.Date, 
         totaldays = Total.Days, 
         spidpres = SpidPres, 
         webpres = WebPres, 
         WebSize = WebSize.cm.)
transplant <- transplant %>%
  rename_all(tolower)
names(transplant)
#4) Add missing data. Note that the people who entered the data did not drag down the island or location column to fill every row. 
transplant_comp <- transplant %>%
  complete(island, site)
transplant_comp <- transplant_comp %>%
  fill(web #) 


#5) Separate "Top color - Bowl color" into two different columns, with the first letter for the top color and the second letter for the bowl color. We do not need to save the original column.
transplant2 <- transplant %>%
  separate(col=`web #`, into=c("web_a", "web_b"), sep="'", remove = T) 


#6) Use the complete function to see if we have data for all 3 transects at each location. Do not overwrite the poll dataframe when you do this. 
transplant_comp <- transplant2 %>%
  complete(web_a, web_b)
transplant_comp <- transplant2 %>%
  complete(Guam)

#which transects appear to be missing, and why?
#Guam because it can't be found

#7) Unite island, site, transect into a single column with no spaces or punctuation between each part. Call this column uniqueID. We need to keep the original columns too. 

transplant2 <- transplant2 %>%
  unite(uniqueID, c(island, site), sep="", remove=F)

#8) Now, make this "wide" dataset into a "long" dataset, with one column for the insect orders, and one column for number of insects. 
transplant2_long <- transplant2 %>%
  gather(key = "insectorder", value = "insectnum")
#9) And just to test it out, make your "long" dataset into a "wide" one and see if anything is different. 
transplant2wide <- transplant2_long %>%
  spread(key = insectorder, value = insectnum)
#are you getting an error? Can you figure out why? 
#keys are shared in the rows and it needs a unique combination
#10) Now, join the "InsectData" with the "CollectionDates" tab on the excel worksheet. You'll need to read it in, and then play around with the various types of 'mutating joins' (i.e. inner_join, left_join, right_join, full_join), to see what each one does to the final dataframe. 
InsectData <- read_csv("data/tidy/InsectData.csv")
CollectionDates <- read_csv("data/tidy/CollectionDates.csv")
leftjoin_InsectData <- transplant2 %>%
  left_join(InsectData, by = c("startdate", "enddate"))