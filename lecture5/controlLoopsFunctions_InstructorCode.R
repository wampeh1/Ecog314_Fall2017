### Instructor Code for Functions/Control/Loops Lecture

#-----
# Necessary Libraries

library(dplyr)
library(tidyr)
library(ggplot2)

#-----
# Read in the data
loanData <- read.csv("../data/loan1.csv", stringsAsFactors = FALSE)

loanData$date <- as.Date(loanData$date, format = "%m/%d/%Y")

#-----
# if/else examples

x <- 35
if(x > 0) {
  print("This number is positive")
}

x <- -2
if(x >= 0) {
  sqrt(x)
} else {
  print("Cannot compute the square root of a negative number")
}

grade <- 85
if(grade >= 90){
  print("A")
} else if(grade >= 80) {
  print("B")
} else if (grade >= 70) {
  print("C")
} else if (grade >= 60) {
  print("D")
} else {
  print("F")
}

#-----
# In class exercise 
player1 <- "paper"
player2 <- "scissors"

if((player1 == "rock" & player2 == "scissors") | (player1 == "paper" & player2 == "rock") | (player1 == "scissors" & player2 == "paper")) {
  print("Player 1 wins!")
} else {
  print("Player 2 wins!")
}

#-----
# Preparing the data for calculating the balance at the end of the grace period
gracePeriodEnds <- as.Date("2016-12-15")

loanData <- loanData %>% mutate(rate = rate / 12 / 100,  
                                currentPrincipal = 0,  
                                age = 0)

loanData[1, "age"] <- length(seq(loanData[1, "date"],
                                 gracePeriodEnds, 
                                 by = "month"))

#-----
# The initial code for the first loan - edit in class to demonstrate the annoying parts of copy/paste
if(loanData[1, "subsidized"] == "NO") {
  
  principal <- loanData[1, "principal"]
  rate <- loanData[1, "rate"]
  age <- loanData[1, "age"]
  
  loanData[1, "currentPrincipal"] <- principal * (1 + rate * age)
  
} else {
  
  loanData[1, "currentPrincipal"] <- loanData[1, "principal"]
}

loanData[1, ]

#-----
# For loop example
boringVector <- c(3, 5, 9, 11, 16, 21, 44)

for(i in c(1, 2, 3, 4, 5, 6, 7)) {
  print(boringVector[i] ^ 2)
}

i <- 1
boringVector[i] ^ 2

i <- 2
boringVector[i] ^ 2

i <- 3
boringVector[i] ^ 2

i <- 4
boringVector[i] ^ 2

i <- 5
boringVector[i] ^ 2

i <- 6
boringVector[i] ^ 2

i <- 7
boringVector[i] ^ 2

for(i in seq(1, 7)) {
  print(boringVector[i] ^ 2)
}

for(i in 1:7) {
  print(boringVector[i] ^ 2) 
}

#-----
# In Class Exercise - Fibonacci Sequence
fibSeq <- c(0, 1)

for(i in 3:15) {
  fibNext <- fibSeq[i - 1] + fibSeq[i - 2]
  
  fibSeq <- c(fibSeq, fibNext)
}

fibSeq

#-----
# For loop to calculate the balance due after the grace period ends
for(i in 1:nrow(loanData)) {
  loanData[i, "age"] <- length(seq(loanData[i, "date"],
                                   gracePeriodEnds, 
                                   by = "month"))
  
  if(loanData[i, "subsidized"] == "NO") {
    
    principal <- loanData[i, "principal"]
    rate <- loanData[i, "rate"]
    age <- loanData[i, "age"]
    
    loanData[i, "currentPrincipal"] <- principal * (1 + rate * age)
    
  } else {
    
    loanData[i, "currentPrincipal"] <- loanData[i, "principal"]
  }
}

#-----
# Calculating the minimum payment
loanData <- mutate(loanData, minPayment = 0)

for(i in 1:nrow(loanData)) {
  principal <- loanData[i, "currentPrincipal"]
  rate <- loanData[i, "rate"]
  
  loanData$minPayment[i] <- principal * (rate / (1 - (1 + rate) ^ -120))
  
}

#-----
# Set up for the monthly balance
loanBalance <- data.frame(month = seq(1:120), curBalance = 0)

loanBalance[1, "curBalance"] <- loanData[1, "currentPrincipal"]
rate <- loanData[1, "rate"]
payment <- loanData[1, "minPayment"]

for(i in 2:nrow(loanBalance)) {
  balance <- loanBalance[i - 1, "curBalance"]
  
  loanBalance[i, "curBalance"] <- balance * (1 + rate) - (payment)
}

#-----
# In Class Exercise - Fixing the negative payment bug
payment <- loanData[1, "minPayment"] + 5

for(i in 2:nrow(loanBalance)) {
  balance <- loanBalance[i - 1, "curBalance"]
  
  
  if(balance <= payment) {
    loanBalance[i, "curBalance"] <- 0
  } else {
    loanBalance[i, "curBalance"] <- balance * (1 + rate) - (payment)
  }
}

#-----
# Example of user-defined functions
calculateRoots <- function(x) {
  if(x >= 0) {
    return(sqrt(x))
  } else {
    return("Cannot compute the square root of a negative number")
  }
}

convertLetterGrade <- function(grade) {
  if(grade >= 90){
    return("A")
  } else if(grade >= 80) {
    return("B")
  } else if (grade >= 70) {
    return("C")
  } else if (grade >= 60) {
    return("D")
  } else {
    return("F")
  }
}

#-----
# In Class Exercise - Fibonacci Function
fibonacci <- function(n = 3) {
  fibSeq <- c(0, 1)
  
  for(i in 3:n) {
    fibNext <- fibSeq[i - 1] + fibSeq[i - 2]
    
    fibSeq <- c(fibSeq, fibNext)
  }
  
  return(fibSeq[1:n])
}

#-----
# In class exercise - Calculate the monthly balance for one loan
calculateMonthlyBal <- function(initialBalance, rate, payment) {
  loanBal <- data.frame(month = seq(1:120), curBal = 0)
  loanBal[1, "curBal"] <- initialBalance
  
  for(i in 2:nrow(loanBal)) {
    balance <- loanBal[i - 1, 2]
    
    if(balance <= payment) {
      loanBal[i, 2] <- 0
    } else {
      loanBal[i, 2] <- balance * (1 + rate) - (payment)
    }
  }
  
  return(loanBal)
}

#-----
# Calculate the monthly balance for all the loans
totalMonthlyBal <- data.frame(month = seq(1:120), balance = 0)

for(i in 1:nrow(loanData)) {
  principal <- loanData[i, "currentPrincipal"]
  rate <- loanData[i, "rate"]
  payment <- loanData[i, "minPayment"]
  
  individualMonthlyBal <- calculateMonthlyBal(principal, rate, payment)
  
  totalMonthlyBal[, "balance"] <- totalMonthlyBal[, "balance"] + individualMonthlyBal[, "curBal"]
}

#-----
# In Class Exercise - Convert the above loop to a function
calcTotalMonthlyBal <- function(loanData, gracePeriodEnds, paymentMod = 0) {
  library(dplyr)
  
  # Make sure the dates are dates
  loanData$date <- as.Date(loanData$date, format = "%m/%d/%Y")
  gracePeriodEnds <- as.Date(gracePeriodEnds)
  
  # Now, get the data frame ready for all the calculations
  loanData <- loanData %>% mutate(rate = rate / 12 / 100, 
                                  currentPrincipal = 0,
                                  age = 0,
                                  minPayment = 0)
  
  # First, calculate the balance after the grace period ends
  for(i in 1:nrow(loanData)) {
    loanData[i, "age"] <- length(seq(loanData[i, "date"],
                                     gracePeriodEnds,
                                     by = "month"))
    
    
    if(loanData[i, "subsidized"] == "NO") {
      principal <- loanData[i, "principal"]
      rate <- loanData[i, "rate"]
      age <- loanData[i, "age"]
      
      loanData[i, "currentPrincipal"] <- principal * (1 + rate * age)
      
    } else {
      loanData[i, "currentPrincipal"] <- loanData[i, "principal"]
    }
  }
  
  # Now, the minimum payment
  for(i in 1:nrow(loanData)) {
    principal <- loanData[i, "currentPrincipal"]
    rate <- loanData[i, "rate"]
    
    loanData$minPayment[i] <- principal * (rate / (1 - (1 + rate) ^ -120))
  }
  
  # Now, the monthly loan balance
  totalMonthlyBal <- data.frame(month = seq(1:120), balance = 0)
  
  for(i in 1:nrow(loanData)) {
    principal <- loanData[i, "currentPrincipal"]
    rate <- loanData[i, "rate"]
    payment <- loanData[i, "minPayment"] + paymentMod / nrow(loanData)
    
    individualMonthlyBal <- calculateMonthlyBal(principal, rate, payment)
    
    totalMonthlyBal[, "balance"] <- totalMonthlyBal[, "balance"] + individualMonthlyBal[, "curBal"]
  }
  
  return(totalMonthlyBal)
}

#-----
# In Class Exercise - Plot the data
library(reshape2)
library(ggplot2)

balMinPayment <- calcTotalMonthlyBal(loanData, gracePeriodEnds = "2016-12-31")

balPlus20 <- calcTotalMonthlyBal(loanData, gracePeriodEnds = "2016-12-31", paymentMod = 20)
colnames(balPlus20) <- c("month", "balance20")

balPlus100 <- calcTotalMonthlyBal(loanData, gracePeriodEnds = "2016-12-31", paymentMod = 100)
colnames(balPlus100) <- c("month", "balance100")

plotBals <- left_join(balMinPayment, balPlus20)
plotBals <- left_join(plotBals, balPlus100)

gatheredPlotBals <- gather(plotBals, key = "variable", value = "value", balance:balance100)

ggplot(gatheredPlotBals, aes(x = month, y = value, color = variable, linetype = variable)) + 
  geom_line()