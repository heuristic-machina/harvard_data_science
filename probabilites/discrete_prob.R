#monte carlo
beads <- rep(c('red', 'blue'), times = c(2,3))
beads
#[1] "red"  "red"  "blue" "blue" "blue"

#pick bead at random
sample(beads, 1)
#replicate() repeats task any number of times i.e. 10000
B <- 10000
events <- replicate(B, sample(beads, 1))

#verify distribution results using table
tab <- table(events)
tab
#events
#blue  red 
#6094 3906 

#proportions table
prop.table(tab)
#events
# blue    red 
#0.6094 0.3906 

#sample() showing default behavior of replace=False
sample(beads, 5)
#> [1] "red"  "blue" "blue" "blue" "red"
sample(beads, 5)
#> [1] "red"  "red"  "blue" "blue" "blue"
sample(beads, 5)
#> [1] "blue" "red"  "blue" "red"  "blue"

#replace=true behaves like replicate()
events <- sample(beads, B, replace = TRUE)
prop.table(table(events))
#> events
#blue    red 
#0.6017 0.3983