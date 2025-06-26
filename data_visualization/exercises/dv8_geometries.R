#8.16 Geometry Exercises

#geom_label()
murders |> ggplot(aes(population, total, label=abb)) + geom_label()

#geom_label monochrome
murders |> ggplot(aes(population, total, label=abb)) + geom_label(color='blue')

#mapping color to label
murders |> ggplot(aes(population, total, label=abb, color=region)) + geom_label()

#14 change the x and y axes to log10 to account for the skewed population distribution
p <- murders |> ggplot(aes(population, total, label=abb, color=region)) + geom_label()
p + scale_x_log10() + scale_y_log10()