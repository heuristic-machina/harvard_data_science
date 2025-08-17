#3.3 monte carlo
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

#3.4 Combinations & Permutations
#using paste to join two small strings
number <- 'Three'
suit <- 'Hearts'
paste(number, suit)
#paste() on pairs of vectors
paste(letters[1:5], as.character(1:5))
#> [1] "a 1" "b 2" "c 3" "d 4" "e 5"
#> expand.grid gives all combinations of two vectors' entries
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))
#>   pants shirt
#> 1  blue white
#> 2 black white
#> 3  blue  grey
#> 4 black  grey
#> 5  blue plaid
#> 6 black plaid
#>expand.grid and paste() in creating deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", 
               +              "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)



beads <- rep(c('red', 'blue'), times = c(2,3))
beads
#[1] "red"  "red"  "blue" "blue" "blue"
sample(beads, 1)
#[1] "blue"
B <- 10000
events <- replicate(B, sample(beads, 1))
tab <- table(events)
tab
events
#blue  red 
#6094 3906 
prop.table(tab)
events
#blue    red 
#0.6094 0.3906 
set.seed(1986)
?set.seed
sample(beads, 5)
#[1] "red"  "blue" "blue" "blue" "red" 
sample(beads, 5)
#[1] "red"  "red"  "blue" "blue" "blue"
sample(beads, 5)
#[1] "blue" "red"  "blue" "red"  "blue"

events <- sample(beads, B, replace = TRUE)
prop.table(table(events))
events
#blue    red 
#0.6017 0.3983 
number <- 'Three'
suit <- 'Hearts'
paste(number, suit)
#[1] "Three Hearts"
letters[1:5]
#[1] "a" "b" "c" "d" "e"
as.character(1:5)
#[1] "1" "2" "3" "4" "5"
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", 
               +              "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)
deck
#[1] "Ace Diamonds"   "Deuce Diamonds" "Three Diamonds" "Four Diamonds" 
#[5] "Five Diamonds"  "Six Diamonds"   "Seven Diamonds" "Eight Diamonds"
#[9] "Nine Diamonds"  "Ten Diamonds"   "Jack Diamonds"  "Queen Diamonds"
#[13] "King Diamonds"  "Ace Clubs"      "Deuce Clubs"    "Three Clubs"   
#[17] "Four Clubs"     "Five Clubs"     "Six Clubs"      "Seven Clubs"   
#[21] "Eight Clubs"    "Nine Clubs"     "Ten Clubs"      "Jack Clubs"    
#[25] "Queen Clubs"    "King Clubs"     "Ace Hearts"     "Deuce Hearts"  
#[29] "Three Hearts"   "Four Hearts"    "Five Hearts"    "Six Hearts"    
#[33] "Seven Hearts"   "Eight Hearts"   "Nine Hearts"    "Ten Hearts"    
#[37] "Jack Hearts"    "Queen Hearts"   "King Hearts"    "Ace Spades"    
#[41] "Deuce Spades"   "Three Spades"   "Four Spades"    "Five Spades"   
#[45] "Six Spades"     "Seven Spades"   "Eight Spades"   "Nine Spades"   
#[49] "Ten Spades"     "Jack Spades"    "Queen Spades"   "King Spades"   
kings <- paste('King', suits)
mean(deck %in% kings)
#[1] 0.07692308
install.packages('gtools')
library(gtools)


permutations(3, 2)
#[,1] [,2]
#[2,]    1    3
#[3,]    2    1
#[4,]    2    3
#[5,]    3    1
#[6,]    3    2
deck <- expand.grid(number = numbers, suit = suits)
deck
#number     suit
#1     Ace Diamonds
#2   Deuce Diamonds
#3   Three Diamonds
#4    Four Diamonds
#5    Five Diamonds
#6     Six Diamonds
#7   Seven Diamonds
#8   Eight Diamonds
#9    Nine Diamonds
#10    Ten Diamonds
#11   Jack Diamonds
#12  Queen Diamonds
#13   King Diamonds
#14    Ace    Clubs
#15  Deuce    Clubs
#16  Three    Clubs
#17   Four    Clubs
#18   Five    Clubs
#19    Six    Clubs
#20  Seven    Clubs
#21  Eight    Clubs
#22   Nine    Clubs
#23    Ten    Clubs
#24   Jack    Clubs
#25  Queen    Clubs

deck <- paste(deck$number, deck$suit)
deck
#[1] "Ace Diamonds"   "Deuce Diamonds" "Three Diamonds" "Four Diamonds" 
#[5] "Five Diamonds"  "Six Diamonds"   "Seven Diamonds" "Eight Diamonds"
#[9] "Nine Diamonds"  "Ten Diamonds"   "Jack Diamonds"  "Queen Diamonds"
#[13] "King Diamonds"  "Ace Clubs"      "Deuce Clubs"    "Three Clubs"   
#[17] "Four Clubs"     "Five Clubs"     "Six Clubs"      "Seven Clubs"   
#[21] "Eight Clubs"    "Nine Clubs"     "Ten Clubs"      "Jack Clubs"    
#[25] "Queen Clubs"    "King Clubs"     "Ace Hearts"     "Deuce Hearts"  
#[29] "Three Hearts"   "Four Hearts"    "Five Hearts"    "Six Hearts"    
#[33] "Seven Hearts"   "Eight Hearts"   "Nine Hearts"    "Ten Hearts"    
#[37] "Jack Hearts"    "Queen Hearts"   "King Hearts"    "Ace Spades"    
#[41] "Deuce Spades"   "Three Spades"   "Four Spades"    "Five Spades"   
#[45] "Six Spades"     "Seven Spades"   "Eight Spades"   "Nine Spades"   
#[49] "Ten Spades"     "Jack Spades"    "Queen Spades"   "King Spades"  

#probability of first card being a king is 1/13 if found using mean()
kings <- paste('King', suits)
mean(deck %in% kings)
#> [1] 0.0769

#find probability of second card being a king,(3/51)
#permutation() computes all possible outcomes when order matters
#returned result is lexographic by default
hands <- permutations(52, 2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
#2652 permutations matrix
first_card
sum(first_card %in% kings)
#> [1] 204
#2652/13 is 204

#Compute what fraction of second card being a king
#sum() and mean() produce same result
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)
#> [1] 0.0588
#> conditional probability equivalent to 3/51
mean(first_card %in% kings & second_card %in% kings) / 
  mean(first_card %in% kings)
#> [1] 0.0588


#Black jack example using combinations() where order does not matter
#Natural 21 where getting an Ace and a face card
aces <- paste('Ace', suits)
facecard <- c('King', 'Queen', 'Jack', 'Ten')
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)
hands <- combinations(52, 2, v = deck)
hands
#1326 returned results

#use mean() to find probability
#this line shows aces as the first card because of how combination() enumerates results
mean(hands[,1] %in% aces & hands[,2] %in% facecard)
#[1] 0.04826546
#alternatively an or statement could be used
mean(hands[,1] %in% aces & hands[,2] %in% facecard) | (hands[,2] %in% aces & hands[,1] %in% facecard)
#[1] 0.04826546

#3.4.1 Monte Carlo example
#using the sample() method without replacement to keep track of Natural 21s obtained
hand <- sample(deck, 2)
hand
#[1] "Four Diamonds"  "Queen Diamonds"

#check for black jack combinations
(hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
#[1] FALSE
#repeat 10,000 times deducing approximation of Natural 21

#formulate function of the above steps
blackjack <- function(){
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) |
    (hand[2] %in% aces & hand[1] %in% facecard)
  }
blackjack()
#> [1] FALSE
 
#>now play the game 10,000 times
B <- 10000
results <- replicate(B, blackjack())
mean(results)
#[1] 0.0468

#3.5.2 Birthday example
#check if at least 2 people share the same bday with a sampling set of 50
#any() duplicated() replicate()
B <- 10000
same_birthday <- function(n){
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
  }
results <- replicate(B, same_birthday(50))
mean(results)
#[1] 0.9684
#create look up table to visualize how the high probability 0.9684 increases as n increases
compute_prob <- function (n, B=10000){
  results <- replicate(B, same_birthday(n))
  mean(results)
}
#use sapply(input object, function to apply input data)
#sapply is r's more efficient loop method returning simplified output by default
n <- seq(1,60)
prob <- sapply(n, compute_prob)
library(tidyverse)