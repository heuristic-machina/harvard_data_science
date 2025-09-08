#Linear Models 17: Treatment Effect Models

#17.4 Contrast
#In the example of finding if diets in male and female mice has an 
#effect on mass

#the variables referred as factors in lm() define the intercept of the
#first level and contrast(the difference) the relationship by the next 
#coefficient:

fit<-lm(body_weight ~ sex, data=mice_weights)
coefficients(fit)
#> (Intercept)        sexM 
#>       29.76        8.82

#intercept b = mu_y - rho(sigma_y/sigma_x)*mu_x
#recovering the mean for males using the sum of the coefficients 
# and (intercept) 
sum(fit$coefficients[1:2])
#[1] 38.5795
names(fit$coefficients[1:2])
#[1] "(Intercept)" "sexM" 

#emmeans() estimated marginal means simplifies calculation
install.packages('emmeans')
library(emmeans)
emmeans(fit, ~sex)
#>  sex emmean    SE  df lower.CL upper.CL
#>  F     29.8 0.339 778     29.1     30.4
#>  M     38.6 0.346 778     37.9     39.3
#> 
#> Confidence level used: 0.95
 
#>Use contrast to represent parameter difference between each group to 
#>the mean
fit<- lm(body_weight ~ sex, data=mice_weights,
         contrasts=list(sex=contr.sum))
coefficients(fit)
#> (Intercept)        sex1 
#>       34.17       -4.41
     
#>the intercept is now larger, reflecting the overall mean rather 
#>#than just the mean for females.  The coefficient for men is not 
#>#shown because it is redundant: beta1=-beta1

#>17.7 Exercises

#1. Once you fit a model, the estimate of the standard error sigma
#can be obtained as follows:
fit <- lm(body_weight ~ diet, data = mice_weights)
summary(fit)$sigma
#[1] 7.658293
#Compute the estimate of using both the model that includes only 
#diet and a model that accounts for sex. Are the estimates the same? 
#If not, why not?
fit2 <- lm(body_weight ~ sex, data = mice_weights)
summary(fit2)$sigma
#[1] 6.765185
#the gender of the mice appears to have a stronger contribution to the 
#observed weight

#2. One of the assumption of the linear model fit by lm is that the 
#standard deviation of the errors epilson(i) is equal for all i. This 
#implies that it does not depend on the expected value. Group the mice 
#by their weight like this:
breaks <- with(mice_weights, seq(min(body_weight), max(body_weight), 1))
dat <- mutate(mice_weights, 
              group = cut(body_weight, breaks, include_lowest = TRUE))

# group > 10
g_10 <- dat %>% count(group) %>% filter(n>10)

#filter dat to g_10 to use in model
dat_filtered<- dat %>% filter(group %in% g_10$group)

#fit model
mod<- lm(body_weight~group, data=dat_filtered)

#get estimated means and se
emm<-emmeans(mod, ~ group)
emm
#sd = se*sqrt(n) for each group
find_sd<-summary(emm) %>% left_join(g_10, by='group') %>%
  mutate(sd=SE*sqrt(n))
find_sd
