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

#3. The dataset also includes a variable indicating which litter 
#the mice came from. Create a boxplot showing weights by litter. 
#Use faceting to make separate plots for each diet and sex combination.
library(ggplot2)

ggplot(dat, aes(x = litter, y = body_weight)) +
  geom_boxplot() +
  facet_grid(sex ~ diet) +  # rows = sex, columns = diet
  labs(
    x = "Litter",
    y = "Body Weight",
    title = "Observed Litter Body Weights"
  )

#4 Use a linear model to test for a litter effect, taking into 
#account sex and diet. Use ANOVA to compare the variability explained
# by litter with that of other factors.
#body_weight=response, litter=predictor, sex+diet=covariates, dat=loci
model<-lm(body_weight ~ litter + sex + diet, data=dat)
summary(model)
#Coefficients:
#               Estimate  Std. Error t value Pr(>|t|)    
#(Intercept)    27.6498     0.4196  65.888  < 2e-16 ***
#  litter2      -1.2383     0.4530  -2.733  0.00641 ** 
#  sexM          8.8394     0.4460  19.821  < 2e-16 ***
#  diethf        5.3276     0.4490  11.866  < 2e-16 ***
anova(model)
#Analysis of Variance Table
#Response: body_weight
#           Df  Sum Sq Mean Sq  F value Pr(>F)    
#litter      1    87.7    87.7   2.2628 0.1329    
#sex         1 15148.1 15148.1 390.7959 <2e-16 ***
#diet        1  5457.6  5457.6 140.7968 <2e-16 ***
#Residuals 776 30079.4    38.8   

#5. The mouse_weights data includes two other outcomes: bone 
#density and percent fat. Create a boxplot illustrating bone 
#density by sex and diet. Compare what the visualizations reveal 
#about the diet effect by sex.
library(ggplot2)

ggplot(dat, aes(x = percent_fat, y = bone_density)) +
  geom_boxplot() +
  facet_grid(sex ~ diet) +  # rows = sex, columns = diet
  labs(
    x = "Percent Fat",
    y = "Bone Density",
    title = "Observed Bone Density and Fat Percentage"
  )

#Fit a linear model and conduct a separate test for the diet 
#effect on bone density for each sex. Note that the diet effect is 
#statistically significant for females but not for males. Then fit 
#the model to the entire dataset that includes diet, sex and their 
#interaction. Notice that the diet effect is significant, yet the 
#interaction effect is not. Explain how this can happen. Hint: To fit 
#a model to the entire dataset with a separate effect for males and 
#females, you can use the formula ~ sex + diet:sex

model_bone<-lm(bone_density ~ sex + diet, data = dat)
summary(model_bone)
#results for males as females are the reference
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.529678   0.006075  87.197  < 2e-16 ***
#  sexM         0.104560   0.007088  14.752  < 2e-16 ***
#  diethf      -0.022378   0.007087  -3.158  0.00165 ** 

#switching the reference to males
dat$sex <- relevel(dat$sex, ref = "M")
#Coefficients:
#             Estimate Std. Error   t value   Pr(>|t|)    
#(Intercept)    0.634238   0.006137 103.344  < 2e-16 ***
#  sexF        -0.104560   0.007088 -14.752  < 2e-16 ***
#  diethf      -0.022378   0.007087  -3.158  0.00165 ** 
model_bone<-lm(bone_density ~ sex + diet:sex, data = dat)
summary(model_bone)
#Coefficients:
#               Estimate Std. Error t value   Pr(>|t|)    
#(Intercept)    0.629404   0.007082  88.878  < 2e-16 ***
#sexF          -0.095038   0.009940  -9.562  < 2e-16 ***
#sexM:diethf   -0.012476   0.010136  -1.231  0.21876    
#sexF:diethf   -0.031827   0.009901  -3.214  0.00136 ** 