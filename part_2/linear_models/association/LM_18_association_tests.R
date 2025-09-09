#Linear Models 18: Association Tests

#Exercises 18.7

#1. A famous athlete boasts an impressive career, winning 70% 
#of her 500 career matches. Nevertheless, this athlete is criticized
# because in important events, such as the Olympics, she has a losing
# record of 8 wins and 9 losses. Perform a Chi-square test to determine
# if this losing record can be simply due to chance as opposed to not 
#performing well under pressure.

# Observed wins and losses
observed <- c(8, 9)

# Expected probabilities under the null (equal chance)
expected_probs <- c(0.5, 0.5)

# Chi-square goodness-of-fit test
chisq.test(observed, p = expected_probs)
#	Chi-squared test for given probabilities
#data:  observed
#X-squared = 0.058824, df = 1, p-value = 0.8084

#outcome due to chance

#2. Why did we use the Chi-square test instead of Fisher’s exact 
#test in the previous exercise?

#Because the Chi-square test runs faster.

#3. Compute the odds ratio of “losing under pressure” along with 
#a confidence interval.

wins <- 8
losses <- 9

odds_losing <- losses / wins
odds_losing
# 1.125

# Log-odds CI using standard error
se_log_or <- sqrt(1/wins + 1/losses)
ci_log_or <- log(odds_losing) + c(-1, 1) * qnorm(0.975) * se_log_or
ci_or <- exp(ci_log_or)
ci_or
#[1] 0.4340532 2.9158295

#4. Notice that the p-value is larger than 0.05, but the 95% 
#confidence interval does not include 1. What explains this?

#p-value of .405 is greater than alpha=.05 and result is outside
#rejection region.  Fail to reject null hypothesis of wins and 
#losses equally likely

#confidence interval includes 1
-.5+qnorm(.975)*se_log_or
#[1] 0.4523713
.5+qnorm(.975)*se_log_or
#[1] 1.452371

#5. Multiply the two-by-two table by 2 and see if the p-value and
# confidence interval are a better match.
-1+qnorm(.975)*se_log_or
#[1] -0.04762869
1+qnorm(.975)*se_log_or
#[1] 1.952371

#6. FIX Use the research_funding_rates data to estimate the log 
#odds ratio along and standard errors comparing women to men for 
#each discipline. Compute a confidence interval and report all the 
#disciplines for which one gender appears to be favored over the 
#other.
library(tidyverse)
library(dslabs)
research_funding_rates |> select(discipline, applications_total, 
                                 success_rates_total)


totals <- research_funding_rates |> 
  select(-discipline) |> 
  summarize_all(sum) |>
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women)
head(totals)
#   yes_men no_men  yes_women   no_women
#1     290   1345       177     1011
totals_percents<- totals |> 
  summarize(
    percent_men=yes_men/(yes_men+no_men),
    percent_women=yes_women/(yes_women+no_women)
    )
totals_percents
#      percent_men percent_women
#1     0.17737     0.1489899

#awards rate
rate <- with(totals, (yes_men + yes_women))/sum(totals)
rate
#> [1] 0.165
#> 
#> chi-square 2x2 data table for each discipline
results <- research_funding_rates %>%
  mutate(
    yes_men   = awards_men,
    no_men    = applications_men - awards_men,
    yes_women = awards_women,
    no_women  = applications_women - awards_women
  ) %>%
  select(discipline, yes_men, no_men, yes_women, no_women) %>%
  rowwise() %>%
  mutate(
    test = list(chisq.test(matrix(c(yes_men, no_men,
                                    yes_women, no_women),
                                  nrow = 2, byrow = TRUE))),
    OR   = (no_women / yes_women) / (no_men / yes_men)
  ) %>%
  mutate(p_value = test$p.value) %>%
  select(-test) %>%
  ungroup()

results
# A tibble: 9 × 6
#discipline          yes_men no_men yes_women no_women chisq_p
#<chr>                 <dbl>  <dbl>     <dbl>    <dbl>   <dbl>
#1 Chemical sciences        22     61        10       29  1     
#2 Physical sciences        26    109         9       30  0.766 
#3 Physics                  18     49         2        7  1     
#4 Humanities               33    197        32      134  0.242 
#5 Technical sciences       30    159        13       49  0.466 
#6 Interdisciplinary        12     93        17       61  0.0902
#7 Earth/life sciences      38    118        18      108  0.0502
#8 Social sciences          65    360        47      362  0.131 
#9 Medical sciences         46    199        29      231  0.0225

#log odds ratio scale
#OR>1 and OR>1 are equally far from 1 in log space
#se simple closed form
#se_log_or=sqrt(1/a+1/b+1/c+1+d) where: a=yes_women,b=no_women,
#c=yes_men,d=no_men


#conf int=get 95% ci from the log OR +- 1.96 x se
#flag: women favored=ci>1, men favored=ci<1, otherwise no difference
library(dplyr)

results <- research_funding_rates %>%
  mutate(
    a = awards_women,
    b = applications_women - awards_women,
    c = awards_men,
    d = applications_men - awards_men,
    OR = (a / b) / (c / d),
    log_OR = log(OR),
    SE_log_OR = sqrt(1/a + 1/b + 1/c + 1/d),
    CI_low = exp(log_OR - 1.96 * SE_log_OR),
    CI_high = exp(log_OR + 1.96 * SE_log_OR),
    favored = case_when(
      CI_low > 1 ~ "Women favored",
      CI_high < 1 ~ "Men favored",
      TRUE ~ "No clear difference"
    )
  ) %>%
  select(discipline, OR, CI_low, CI_high, favored)

results
#discipline        OR    CI_low   CI_high             favored
#1   Chemical sciences 0.9561129 0.4011820 2.2786462 No clear difference
#2   Physical sciences 1.2576923 0.5327356 2.9691837 No clear difference
#3             Physics 0.7777778 0.1476269 4.0977512 No clear difference
#4          Humanities 1.4255993 0.8361760 2.4305091 No clear difference
#5  Technical sciences 1.4061224 0.6807863 2.9042599 No clear difference
#6   Interdisciplinary 2.1598361 0.9642179 4.8380060 No clear difference
#7 Earth/life sciences 0.5175439 0.2788050 0.9607132         Men favored
#8     Social sciences 0.7190820 0.4807431 1.0755828 No clear difference
#9    Medical sciences 0.5431018 0.3287783 0.8971383         Men favored