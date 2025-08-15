#monte carlo
beads <- rep(c('red', 'blue'), times = c(2,3))
beads
#[1] "red"  "red"  "blue" "blue" "blue"

#pick bead at random
sample(beads, 1)