herb <- herbivory_organized %>%
  write.csv
herbivory_organized$`File name (new)` <- NULL
herbivory_organized$description <- NULL
herbivory_organized(is.na(data)) == NA]
herbiv <- herbivory_organized %>%
  na.omit(`percent herbivory`)
herbiv <- herbiv %>%
  unite(uniqueID, c(`file name (original)`, Leaf), sep = "", remove = F)

herbiv <- herbiv %>%
  str(herbiv$Date)

leafarea <- herbiv %>%
  select(uniqueID, species, Island, Location,'percent herbivory')
leafarea <- leafarea %>%
  rename(Herbivory = 'percent herbivory')