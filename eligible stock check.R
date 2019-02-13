library(quantmod)
library(dplyr)

#############################################
    #This chunk of code was for randomly generating a list of companies in the industry

  #Lists of possible companies downloaded from nasdaq.com
tech <- read.csv("tech.csv", header=TRUE)
transport <- read.csv("transportation.csv", header=TRUE)
energy <- read.csv("energy.csv", header=TRUE)

  #sample random numbers
id <- sample(1:nrow(transport), 10, replace=FALSE)
companies <- transport[id,]
symbol <- transport[id,1]
symbol

  #Check if each company has data going back to 2008
getSymbols('CVX', from = "2008-01-03", to = "2018-01-03")
head(CVX)
tail(CVX)
#############################################
