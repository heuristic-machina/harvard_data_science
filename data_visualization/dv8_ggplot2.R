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

#8.9 improving and testing plots
p0 <- murders |> ggplot(aes(population/10^6, total))
#scatter plot layer
p1 <- p0 + geom_point(aes(color = region), size = 3)
#labels modifying nudge for upcoming logarithmic application
p2 <- p1 + geom_text(aes(label = abb), nudge_x = 0.1)

#8.10 scales
#log scale
p3 <- p2 + scale_x_log10() + scale_y_log10()

#8.11 annotations
#labs() permits adding title, subtitle, caption
p4 <- p3 + labs(title = 'US Gun Murders in 2010',
                x = 'Population in millions (log scale)',
                y = 'Total number of murders (log scale)',
                color = 'Region')

#avg murder rate uses y = rx, where y and x are the axes
#in log-scale: log(y) = log(r) + log(x), a line with slope 1 and intercept log(r)
r <- murders |>
  summarize(rate = sum(total)/sum(population)*10^6) |>
              pull(rate)

#geom_abline() supplies intercept(a) and slope(b)
#default line has slope 1 and intercept 0
#define intercept
p5 <- p4 +
  geom_abline(intercept = log10(r), lty = 2, color = 'red')

#8.12 add-on packages
#ggthemes
library(ggthemes)
p6 <- p5 + theme_economist()

#8.13 putting it all together
library(ggthemes)
library(ggrepel)

r <- murders |> 
  summarize(rate = sum(total) /  sum(population) * 10^6) |>
  pull(rate)

murders |> 
  ggplot(aes(population/10^6, total)) +   
  geom_abline(intercept = log10(r), lty = 2, color = "red") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel(aes(label = abb)) + 
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "US Gun Murders in 2010",
       x = "Populations in millions (log scale)", 
       y = "Total number of murders (log scale)",
       color = "Region") +
  theme_economist()

#8.14 geometries
#8.14.1 barplots geom_bar() vs. geom_col()
murders |> ggplot(aes(region)) + geom_bar()

#when a table already present using geom_col()
tab <- murders |> 
  count(region) |> 
  mutate(proportion = n/sum(n))
tab |> ggplot(aes(region, proportion)) + geom_col()

#8.14.2 histogram 
#geom_histogram()
heights |> filter(sex == 'Female') |>
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, fill = 'blue', col = 'black')

#8.14.3 density plots
#geom_density()
heights |> filter(sex == 'Female') |> ggplot(aes(height)) + geom_density(fill = 'blue')

#8.14.4 boxplots
heights |> ggplot(aes(sex, height)) + geom_boxplot()
