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
library(emmeans)
emmeans(fit, ~sex)
#>  sex emmean    SE  df lower.CL upper.CL
#>  F     29.8 0.339 778     29.1     30.4
#>  M     38.6 0.346 778     37.9     39.3
#> 
#> Confidence level used: 0.95