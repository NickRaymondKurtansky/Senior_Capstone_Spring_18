#Modified Line Graph Creation
#Edited - Nick Kurtansky - 4-21-2018 : 3:42 PM

#load any necessary libraries
library(dplyr)
library(ggplot2)
library(boot)

#load the stock data we built specifically for the purpose of inputting into the algorithm
load('Inputs.RData')
finance <- t(Inputs$Finance)
health <- t(Inputs$Healthcare)
trans <- t(Inputs$Transportation)
tech <- t(Inputs$Technology)
energy <- t(Inputs$Energy)

industry.matrix <- list(finance, health, trans, tech, energy)
industry.names <- c("Finance", "Healthcare", "Transportation", "Technology","Energy")
investments <- c(5000, 10000, 25000, 50000, 100000)
etas <- c(.05, 1, 20, 60, 150)
fees <- c(0, 4.95)

for(i in 1:5){  #industy
  for(j in 1:5){  #investment
    for(k in 1:5){  #eta
      for(l in 1:2){  #fee
        EG.investment(investments[j], industry.matrix[[i]], fees[l], etas[k], industry.names[i])
      }
    }
  }
}
