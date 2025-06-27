#Data Visualization 10.11 Exercises

#1 Reproduce the image plot we previously made but for smallpox. For this plot,
#do not include years in which cases were not reported in 10 or more weeks.
library(tidyverse)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)
names(us_contagious_diseases)

dat <- us_contagious_diseases |>
  filter(!state%in% c('Hawaii', 'Alaska') 
         & disease == the_disease & weeks_reporting >= 10) |>
  mutate(rate=count/population*10000*52/weeks_reporting) |>
  mutate(state=reorder(state, ifelse(year<=1963, rate, NA), 
                       median, na.rm=TRUE))
dat |> ggplot(aes(year, state, fill=rate)) +
  geom_tile(color='grey50') + scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans='sqrt') +
  geom_vline(xintercept=1963, col='blue') + theme_minimal() +
  theme(panel.grid=element_blank(), legend.position='bottom', 
        text=element_text(size=8)) + labs(title=the_disease, x='', y='')