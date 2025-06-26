#ggplot2 package
library(dplyr)
library(ggplot2)
#creating the object
murders |> ggplot()
#aesthetics
#adding point layer
murders |> ggplot() + geom_point(aes(population/10^6, total))
#adding label layer for each point
murders |> ggplot() + geom_point(aes(population/10^6, total)) + 
  geom_text(aes(population/10^6, total, label = abb))
#object variables must be callable in aes workspace

#8.6 global aesthetic mappings
#mapping allows a dry version of the above code
murders |> ggplot(aes(population/10^6, total)) +
  geom_point() +
  geom_text(aes(label = abb))

#override global mapping example
murders |> ggplot(aes(population/10^6, total)) +
  geom_point() +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))

#8.7 non-aesthetic arguments
#nudge
murders |> ggplot(aes(population/10^6, total)) +
  geom_point(size = 3) +
  geom_text(aes(label = abb), nudge_x = 1.5)

#8.8 categories as colors
murders |> ggplot(aes(population/10^6, total)) +
  geom_point(aes(color = region), size = 3) 