# EEOB590A
# Data_visualization practice exercise
# Please use ggplot2 for all graphing below
# And don't forget to use your dplyr functions like filter, group_by, summarize, and select
library(tidyverse)
library(ggplot2)

#1) Read in poll_long_tidy.csv
poll <- poll_long_tidy %>%
#2) Use table and dplyr to calculate the number of top colors and bowl colors at each transect. Each transect was supposed to have each topcolor represented once, with all four (b, r, w, and y) bowl colors under each top color. 
with(pollweek2, ftable(uniqueID, topcolor, bowlcolor))
bowl <- pollweek2 %>%
  filter(insectorder == "Diptera") %>%
  group_by(uniqueID) %>%
  count(topcolor) %>%
  filter(n != 4)
#3a) Make a histogram for numinsects. 
ggplot(pollweek2, aes(numinsects))+
  geom_histogram()


#3b) Make another histogram for numinsects but omit all rows with 0 insects
ggplot(pollweek2[pollweek2$numinsects >0], aes(numinsects))+
  geom_histogram()

#4a) Make a barplot to show the counts for each level of the site. Were sites evenly sampled? 
ggplot(pollweek2, aes(site))+
  geom_bar()

#4b) Make a graph to visualize the duration pan traps were left out, and use a table to do the same thing. Is there any variation in duration that we will need to account for in a model? 
ggplot(pollweek2, aes(site, duration))+
  geom_boxplot()
with(pollweek2, table(site, duration))
#5) Figure out the top 6 most abundant orders (hint, use the top_n() function; google it for more info). Then, filter the original dataset so you only have those orders. Then, create boxplots of the number of insects per order with different colors indicating the island. Do you notice any general trends in insect abundances by islands? 
toporders <- pollweek2 %>%
  group_by(insectorder) %>%
  summarise(totalinsects = sum(numinsects, na.rm = T)) %>%
  arrange(desc(totalinsects)) %>%
  top_n(6, totalinsects)

polltoporders <- pollweek2 %>%
  filter(insectorder == "Lepidoptera" | insectorder == "Diptera" | insectorder == "Formicidae" | insectorder == "Hemiptera" | insectorder == "Apoidea" | insectorder == "Crabronidae")

ggplot(polltoporders, aes(insectorder, numinsects, color=island))+
  geom_boxplot()
#6) Use the top 6 orders dataset you created above, along with the facet_grid argument to create a set of graphs with insect order on the x axis, number of insects on the y, and rows of graphs for each topcolor and columns of graphs for the island. 
ggplot(polltoporders, aes(insectorder, numinsects))+
  geom_boxplot()+
  facet_grid(topcolor~island)
