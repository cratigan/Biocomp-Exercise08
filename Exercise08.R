rm(list=ls())

##Question 1
#Using the score-by-score information from
#the game summarized in “UWvMSU_1-22-13.txt” generate a graph, similar to one on assignment. 
#Focus more on the control structures required in your script used to
#generate the plot.

#Tips: on this one:
#You’ll want to generate a matrix or dataframe with a cumulative score for each team whenever either
#team scores.
#For plotting purposes, keep it simple. There is a function plot(x,y,type='l') in base package, where
#x and y are vectors and type=‘l’ specifies a line graph. You can add a second line to this graph with
#lines(x,y). Use the help file for these functions to figure out other argument to customize the line
#types if you would like.

data<-read.table(file="UWvMSU_1-22-13.txt", header=TRUE)

cumu_score<-function(x){
  # x should be a data.frame with scores in "score" col
  # returns a list of cumulative scores
  
  result<-numeric(dim(x)[1]) # Initialize output vector
  for (i in 1:length(result)){
    result[i]<-sum(x$score[1:i])
  } # End i loop
  return(result)
}

# separate into 2 new data.frames by team:
uw<-data[data$team=="UW",]
msu<-data[data$team=="MSU",]

# get the cumulative score for each team
uw_cumu<-cumu_score(uw)
msu_cumu<-cumu_score(msu)

# plot cumulative score over time for each team
plot(uw$time, uw_cumu,type='l')
lines(msu$time,msu_cumu)

###Question 2
#Write a game called “guess my number”. The computer will generate a random number 
#between 1 and
#100. The user types in a number and the computer replies “lower” if 
#the random number is lower than the
#guess, “higher” if the random number is higher, and “correct!” 
#if the guess is correct. The player can continue
#guessing up to 10 times

#generate random number
randnum <- sample(c(1:100), 1)
#let user input first guess
guess <- as.integer(readline("I have picked one number between 1&100, you have 10 guesses: "))
#for loop
for(n in 1:10){
  if(guess < randnum){
    print("Higher")
    if(n < 10){
      guess <- as.integer(readline("Try again: "))
    }
    if(n == 10){
      print("You used up all 10 guesses")
    }
  }else if(guess > randnum){
    print("Lower")
    if(n < 10){
      guess <- as.integer(readline("Try again: "))
    }
    if(n == 10){
      print("SYou used up all 10 guesses")
    }
  }else if(guess==randnum){
    print("Correct!")
    break
  }
}  

