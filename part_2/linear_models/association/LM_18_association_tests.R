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