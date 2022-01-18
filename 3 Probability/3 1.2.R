number <- "Three"
suit <- "Hearts"
paste(number, suit)
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck
deck <- paste(deck$number, deck$suit)
deck

kings = paste("King",suits)
mean(deck %in% kings)

library(gtools)
permutations(5,2)
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index = sample(n,5)
all_phone_numbers[index,]

hands <- permutations(52,2, v = deck)
a = nrow(hands)
a
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard
facecard <- paste(facecard$number, facecard$suit)
facecard
hands <- combinations(52, 2, v=deck)
hands
mean(hands[,1] %in% aces & hands[,2] %in% facecard)
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
results
mean(results == FALSE)

bday = seq(1,365)
bday
results = replicate(B,{ 
  bdays = sample(1:365,50, replace=TRUE) 
  any(duplicated(bdays))
})

mean(results)

exact_prob = function(n) {
 p_uni = seq(365,365-n+1)/365
 1-prod(p_uni)
}

n <- seq(1, 60)
e_prob = sapply(n,exact_prob )
plot(n,e_prob)

cp =  function(B){
  results = replicate(B,{ 
    bdays = sample(1:365,22, replace=TRUE) 
    any(duplicated(bdays))
  })
}
B = 10^seq(1,5,len=100)
ep = sapply(B,cp)
print(ep)
plot(log10(B),ep, type="1")


B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l") 
