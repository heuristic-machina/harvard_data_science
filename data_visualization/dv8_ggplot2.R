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