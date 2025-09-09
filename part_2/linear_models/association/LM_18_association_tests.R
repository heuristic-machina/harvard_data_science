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


#7. Divide the log odds ratio estimates by their respective standard
# errors and generate a qqplot comparing these to a standard normal.
# Do any of the disciplines deviate from what is expected by chance?

#z=log(OR)/se_log(OR)
results <- research_funding_rates %>%
  mutate(
    a = awards_women,
    b = applications_women - awards_women,
    c = awards_men,
    d = applications_men - awards_men,
    OR = (a / b) / (c / d),
    log_OR = log(OR),
    SE_log_OR = sqrt(1/a + 1/b + 1/c + 1/d),
    z_score = log_OR / SE_log_OR
  )
library(ggplot2)

ggplot(results, aes(sample = z_score)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(
    title = "QQ Plot of Wald z-scores for Gender Funding Odds Ratios",
    x = "Theoretical Quantiles (Standard Normal)",
    y = "Observed Quantiles"
  )

#modified labeling for visually favored by discipline
library(dplyr)
library(ggplot2)
library(ggrepel)

# Step 1: Compute log OR, SE, and z-scores
results <- research_funding_rates %>%
  mutate(
    a = awards_women,
    b = applications_women - awards_women,
    c = awards_men,
    d = applications_men - awards_men,
    log_OR = log((a / b) / (c / d)),
    SE_log_OR = sqrt(1/a + 1/b + 1/c + 1/d),
    z_score = log_OR / SE_log_OR,
    direction = ifelse(log_OR > 0, "Favors women", "Favors men")
  )

# Step 2: Precompute QQ plot coordinates
qq_data <- qqnorm(results$z_score, plot = FALSE)
qq_df <- data.frame(
  theoretical = qq_data$x,
  sample = qq_data$y,
  discipline = results$discipline,
  direction = results$direction
)

# Step 3: Pick most extreme points for labeling
label_df <- qq_df %>%
  slice_max(order_by = abs(sample), n = 3)

# Step 4: Plot with direction coloring
ggplot(qq_df, aes(x = theoretical, y = sample, color = direction)) +
  geom_point(size = 2.5) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  geom_text_repel(
    data = label_df,
    aes(label = discipline),
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Favors women" = "blue", "Favors men" = "orange")) +
  labs(
    title = "QQ Plot of Wald z-scores for Gender Funding Odds Ratios",
    subtitle = "Blue = odds favor women, Orange = odds favor men",
    x = "Theoretical Quantiles (Standard Normal)",
    y = "Observed Quantiles",
    color = "Direction of effect"
  ) +
  theme_minimal()

#8&9
#Compute an odds ratio comparing Android to iPhone for each 
#sentiment and add it to the table.  Compute a 95% confidence 
#interval for each odds ratio.
library(tidyverse)
library(dslabs)
sentiment_counts

#OR>1 more likely android tweets
#OR<1 more likely iPhone tweets
library(dplyr)

# Totals for each platform
total_android <- sum(sentiment_counts$Android)
total_iphone  <- sum(sentiment_counts$iPhone)

sentiment_or <- sentiment_counts %>%
  mutate(
    # Counts for "absent" category
    Android_no = total_android - Android,
    iPhone_no  = total_iphone - iPhone,
    
    # Odds ratio: (a/b) / (c/d)
    OR = (Android / Android_no) / (iPhone / iPhone_no),
    
    # Log OR and SE for CI
    log_OR = log(OR),
    SE_log_OR = sqrt(1/Android + 1/Android_no + 1/iPhone + 1/iPhone_no),
    
    CI_low  = exp(log_OR - 1.96 * SE_log_OR),
    CI_high = exp(log_OR + 1.96 * SE_log_OR)
  )

head(sentiment_or)
# A tibble: 6 × 10
#sentiment    Android iPhone Android_no iPhone_no    OR log_OR
#<chr>          <int>  <int>      <int>     <int> <dbl>  <dbl>
#1 anger            962    527      21047     17141  1.49 0.397 
#2 anticipation     917    707      21092     16961  1.04 0.0421
#3 disgust          639    314      21370     17354  1.65 0.502 


#10.Generate a plot showing the estimated odds ratios along with their 
#confidence intervals.
library(tidyverse)
library(ggrepel)

# Totals for each platform
total_android <- sum(sentiment_counts$Android)
total_iphone  <- sum(sentiment_counts$iPhone)

# Compute ORs and CIs
sentiment_or <- sentiment_counts %>%
  mutate(
    Android_no = total_android - Android,
    iPhone_no  = total_iphone - iPhone,
    OR = (Android / Android_no) / (iPhone / iPhone_no),
    log_OR = log(OR),
    SE_log_OR = sqrt(1/Android + 1/Android_no + 1/iPhone + 1/iPhone_no),
    CI_low  = exp(log_OR - 1.96 * SE_log_OR),
    CI_high = exp(log_OR + 1.96 * SE_log_OR),
    direction = ifelse(OR > 1, "Favors Android", "Favors iPhone")
  )

# Plot
ggplot(sentiment_or, 
       aes(x = OR,y = reorder(sentiment, OR),                        color = direction)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_low,
                     xmax = CI_high),
                 height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("Favors Android" = "blue",
                                "Favors iPhone" = "orange")) +
  labs(
    title = "Odds Ratios of Sentiment: Android vs. iPhone Tweets",
    x = "Odds Ratio (log scale)",
    y = "Sentiment",
    color = "Direction"
  ) +
  scale_x_log10() +
  theme_minimal()

#11. FIX Test the null hypothesis that there is no difference 
#between tweets from Android and iPhone and report the 
#sentiments with p-values less than 0.05 and more likely to 
#come from Android.
library(dplyr)

# Totals for each platform
total_android <- sum(sentiment_counts$Android)
total_iphone  <- sum(sentiment_counts$iPhone)

android_favored <- sentiment_counts %>%
  rowwise() %>%
  mutate(
    # Build 2×2 table for this sentiment
    table_2x2 = list(matrix(
      c(Android, total_android - Android,
        iPhone,  total_iphone  - iPhone),
      nrow = 2, byrow = TRUE,
      dimnames = list(
        Platform = c("Android", "iPhone"),
        Sentiment = c("Present", "Absent")
      )
    )),
    # Chi-square test (could swap to fisher.test if counts are small)
    p_value = chisq.test(table_2x2)$p.value,
    # Odds ratio
    OR = (Android / (total_android - Android)) /
      (iPhone  / (total_iphone  - iPhone))
  ) %>%
  ungroup() %>%
  filter(p_value < 0.05, OR > 1) %>%
  arrange(desc(OR)) %>%
  select(sentiment, OR, p_value)

android_favored
# A tibble: 6 × 3
#sentiment    OR  p_value
#<chr>     <dbl>    <dbl>
#1 disgust    1.65 4.22e-13
#2 anger      1.49 5.84e-13
#3 negative   1.46 1.60e-19
#4 sadness    1.42 3.05e-10
#5 fear       1.33 1.01e- 6
#6 surprise   1.17 2.46e- 2

#12 For each sentiment, find the words assigned to that 
#sentiment, keep words that appear at least 25 times, 
#compute the odd ratio for each, and show a barplot for 
#those with odds ratio larger than 2 or smaller than 1/2.

#dslabs has the aggregated data.
#get nrc to unnest tokens for the sentiment dataset
library(tidyverse)
library(tidytext)
library(dslabs)

# 1. NRC sentiment lexicon
nrc <- get_sentiments("nrc")

# Total words per sentiment
total_per_sentiment <- tweet_words %>%
  count(sentiment, name = "total_words")

# Word counts per sentiment
word_counts <- tweet_words %>%
  count(sentiment, word, name = "count")

# Join totals and compute OR
word_or <- word_counts %>%
  left_join(total_per_sentiment, by = "sentiment") %>%
  group_by(word) %>%
  mutate(
    a = count,
    b = total_words - count,
    c = sum(count) - count,
    d = sum(total_words) - total_words - c,
    OR = (a / b) / (c / d),
    log_OR = log(OR),
    SE_log_OR = sqrt(1/a + 1/b + 1/c + 1/d),
    CI_low = exp(log_OR - 1.96 * SE_log_OR),
    CI_high = exp(log_OR + 1.96 * SE_log_OR)
  ) %>%
  ungroup()

extreme_words <- word_or %>%
  filter(OR > 2 | OR < 0.5)


library(ggplot2)

extreme_words %>%
  group_by(sentiment) %>%
  slice_max(order_by = abs(log_OR), n = 5) %>%
  ggplot(aes(x = reorder(word, OR), y = OR, fill = OR > 1)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "orange"),
                    labels = c("Underrepresented", "Overrepresented")) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(
    title = "Words with Extreme Odds Ratios by Sentiment",
    x = "Word",
    y = "Odds Ratio",
    fill = "Direction"
  ) +
  theme_minimal()



