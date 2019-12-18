########## R introduction Part 3 (Thursday, September 12) ############

#Today we will focus on reading in datasets, with some added complexity, and 
#changing the class of variables

#1) Reading in CSV files

#Read in the "transplant_raw.csv" file
transplant_raw <- read.csv("C:/Users/Marjorie/Downloads/Marjorie Final Project/Documents/transplant_raw.csv")
#Read it in again, but assign better column names using principles
#we discussed in class
transplant_raw <- read.csv("C:/Users/Marjorie/Downloads/Marjorie Final Project/Documents/transplant_raw.csv", col_names = c("island", "site", "webnum", "native", "netting", "startdate", "enddate", "totaldays", "spidpres", "webpres", "websizecm"))
#2) Reading in Excel files

#Read in the "leaf damage ag expt.xls" file. This is the exact file I found from 
#an undergraduate project in 2007. It's not perfect. Let's see how we can fix it. 
library(readxl)
leaf_damage_ag_expt <- read_excel("C:/Users/Marjorie/Downloads/Marjorie Final Project/Documents/leaf damage ag expt.xls")
View(leaf_damage_ag_expt)
#read in the "beans" and "herbivory" worksheets and give each the same name 
#as the worksheet tab they came from
beans <- read_excel("C:/Users/Marjorie/Downloads/Marjorie Final Project/Documents/leaf damage ag expt.xls")
herbivory <- read_excel("C:/Users/Marjorie/Downloads/Marjorie Final Project/Documents/leaf damage ag expt.xls", sheet = 2)
  
# Look at the structure of 'beans'. What class are each of the columns in? 
str(beans)
#character
#Read in the beans worksheet again, and tell R the appropriate class/column type 
#for each column. Note that read_excel doesn't let you choose factor, so use text instead
beans <- read_excel("C:/Users/Marjorie/Downloads/Marjorie Final Project/Documents/leaf damage ag expt.xls", 
                    col_types = c("text", "text", "text", 
                                  "numeric", "numeric", "text", "numeric", 
                                  "text", "numeric", "text"))

#After you read it in, you realize that the "Number" column indicates the ID 
#of each exclosure, and therefore should be a factor. Change that column to a
#factor. 
beans$Number <- as.factor(beans$Number)
#check the number of levels for this factor to make sure it converted correctly
levels(beans$Number)
#Notice that there are some X's in the leaflet columns of the herbivory worksheet?
#Read it in again, but this time tell R that the X means NA
herbivory <- read_excel("C:/Users/Marjorie/Downloads/Marjorie Final Project/Documents/leaf damage ag expt.xls", sheet = 2,
                        na = c("", "NA", "X", "x"))


#ADVANCED: Read the herbivory datasheet in again, but omit columns J-L because
#they do not belong with rows 2-6. If you aren't sure how to do this, look
#at the help file for the function that reads in excel files
herbivory <- read_excel("C:/Users/Marjorie/Downloads/Marjorie Final Project/Documents/leaf damage ag expt.xls", sheet = 2,
                        na = c("", "NA", "X", "x"), range = "herbivory!A1:I718")


#3) Dealing with dates

#Read in the "Exploring_dates.xlsx" file. 
Exploring_dates <- read_excel("C:/Users/Marjorie/Downloads/Marjorie Final Project/Documents/Exploring_dates.xlsx")

#Is this object a vector, matrix, dataframe, tibble, array or list?
#dataframe

#The name of this object is kinda unwieldy. Rename the object "dates"
dates <- Exploring_dates

#What format does R recognize each of the dates as? Which ones did not read in 
#as dates? 
str(dates)
#4 and 6
#Create a new column based on the difference in time between date 1 and date 2 
#and name it "duration
duration <- dates$date1 - dates$date2
#ADVANCED: Change the columns that didn't read in correctly into proper dates

