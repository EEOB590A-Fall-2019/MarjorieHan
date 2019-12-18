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


ggplot(leafarea, aes(Herbivory))+
  geom_histogram()
newleaf <- leafarea %>%
  group_by(species)
newleaf <- newleaf %>%
  group_by(Herbivory)
newleaf <- newleaf %>%
  mutate_at(vars(species), numeric)
#What species results in the most herbivory?

ggplot(newleaf, aes(species, Herbivory))+
  geom_boxplot()
dotchart(newleaf$Herbivory)
ggplot(newleaf, aes(species, Herbivory))+
  geom_boxplot()+
  facet_grid(.~Location)

leafanalysis <- lm


herbaglaia <- rnorm

with(leafarea, table(Herbivory, species))
create_report(leafarea)
