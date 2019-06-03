#Question 1: Simuating a Dice Roll
##function that simulates rolling n number of dice

Dicetimes <- function(n) { sample(1:6, n, replace = TRUE)
  
  #create an empty vector
  resDice2 <- c()
  
  #function that takes the n number of dice to roll and finds rolls that sum to y
  ProdSDFind <- function(n, y) { if(sum(Dicetimes(n)) != y) { print("Nope") } else {resDice2 <- Dicetimes(n)}} 
  
  #finding the product and sd of the dice rolls 
  prod(resDice2)
  32400
  3.85176E+20
  
  sd(resDice2)
  1.356203
  1.538618516
  