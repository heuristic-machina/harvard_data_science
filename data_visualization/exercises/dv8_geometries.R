#8.16 Geometry Exercises

#geom_label()
murders |> ggplot(aes(population, total, label=abb)) + geom_label()

#geom_label monochrome
murders |> ggplot(aes(population, total, label=abb)) + geom_label(color='blue')

#mapping color to label
murders |> ggplot(aes(population, total, label=abb, color=region)) + geom_label()