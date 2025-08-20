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

#15 add a title
p + scale_x_log10() + scale_y_log10() + ggtitle('Gun murder data')

#18-20 assign heights data to ggplot object
#assign height to x values through aes
h <- heights |> ggplot(aes(x=height))
h + geom_histogram()
#correcting for binwidth
h + geom_histogram(binwidth=1)

#21 render smooth density in one line
heights |> ggplot(aes(height)) + geom_density()

#22 ggplot(aes(group)) function for examining two groups
heights |> ggplot(aes(height, group = sex)) + geom_density()

#23 ggplot(aes(color)) behaves similarly to group
heights |> ggplot(aes(height, color = sex)) + geom_density()

#24 fill feature
heights |> ggplot(aes(height, fill = sex)) + geom_density()
#blend out overlap
heights |> ggplot(aes(height, fill = sex)) + geom_density(alpha=0.2)