ggplot(newleaf, aes(species, Herbivory))+
  geom_boxplot()
dotchart(newleaf$Herbivory)
ggplot(newleaf, aes(species, Herbivory))+
  geom_boxplot()+
  facet_grid(.~Location)