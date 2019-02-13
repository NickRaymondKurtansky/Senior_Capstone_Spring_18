#Modified EG.revised Eta Algorithm
#Edited - Nick Kurtansky - 4-20-2018 : 4:15 PM

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

  #custom EG function
#########################################################################################################
# Function implementing EG eta update algorithm after normalizing so max(X[,t])\[Congruent]1.
# Keyed to Helmbold, Schapire, et. al. paper.
# EG eta for growth of fortune in an evolving portfolio on n investments
# Goal: Approximately match max with respect to u of product u\[Bullet]X[,1] ... u\[Bullet]X[,T].
# This goal is best with hindsight of the earnings of all constant rebalanced portfolios.
# X is matrix of price relatives. We will automatically redefine X[i,1]=1 to begin with.

EG.investment=function(initial.investment, matrix.of.stock.prices, fee=4.95, eta=sqrt(8*(alpha^2)*log(n)/((n^2)*T))){
  
    #per-transaction brokerage fee

    #keep track of fortune at each time
  purse <- rep(NA, ncol(matrix.of.stock.prices))
  purse[1] <- initial.investment
  
    #re-name the input: x
  stocks=matrix.of.stock.prices
  
    # n = # of stocks
  n=length(stocks[,1])
    # # of transaction periods
  transactions=length(stocks[1,])
  p <- transactions
  
    #stocks relative to value at t=0
  growth <- matrix(NA, n, p)
  for(i in 1:p){
    growth[,i] <- stocks[,i] / stocks[,1]
  }
  
    #initialize price relative matrix
  X <- matrix(NA, n, p-1)
    #retrospectively create the price relative matrix
  for(i in 1:(p-1)){
    X[,i] <- stocks[,i+1]/stocks[,i]
  }

    # T = # of price relatives
  T=length(X[1,])
  
    #alpha
  alpha=((n^2)*log(n)/(8*T))^(1/4)
    #eta: adjustment parameter

#    #print the number of stocks and number of time periods
#  print(c(n,T))
#    #print alpha and eta
#  print(c(alpha,eta))
  
    #m is the vector of highest price-relative at each column
  m=apply(X,2,max)
    #normalize: ratio of individual price-relatives to the highest price-relative at each column 
  normalize=function(v){
    v/max(v)
  }
    #X.normalized = the normalization ratio to all columns
  X.normalized=apply(X,2,normalize)
    #
  X.normalized.tilde=(1-alpha/n)*X.normalized+(alpha/n)
  
    #the natural log of input matrix
  log.prod.X=log(X)
    #initialize: vector of LOG RETURNS
  log.prod.EG.normalized.tilde.returns=rep(0,T)
  
    #initialize: the weights at each time
  P.normalized.tilde=X
    #initialize: UNIFORM WEIGHTS TO BEGIN
  P.normalized.tilde[,1]=rep(1/n,n)
  
  
    #after fees, how much money is available to be spent on buying stock?
  buying_power <- purse[1] - n*fee
    #initialize matrix of the number of shares (in $) we own in each company at each time
  money_portfolio <- matrix(NA, n, p)
  money_portfolio[,1] <- buying_power * P.normalized.tilde[,1]
    #initialize matrix of the number of shares (in #) we own in each company at each time
  share_portfolio <- matrix(NA, n, p)
  share_portfolio[,1] <- money_portfolio[,1] / stocks[,1]
  
  
    #"sit and wait" strategy-invest individually in each individual stock
  number.of.shares.owned <- rep(NA, n)
  number.of.shares.owned <- (purse[1]-1*fee) / as.vector(stocks[,1])
  sit.and.wait <- number.of.shares.owned * stocks[]
  sit.and.wait[,p] <- sit.and.wait[,p] - fee
    
  
    #number of iterations = 120-1 (b/c we've already initialized the first set of weights)
  for(t in 2:(T)){
      #sell entire portfolio
    purse[t] <- sum(share_portfolio[,t-1] * stocks[,t]) - n*fee
    buying_power <- purse[t] - n*fee
    
      #determine the weights vector at each time
    P.normalized.tilde[,t]=P.normalized.tilde[,t-1]*exp(eta*X.normalized.tilde[,t-1]/(P.normalized.tilde[,t-1]%*%X.normalized.tilde[,t-1]))
    P.normalized.tilde[,t]=P.normalized.tilde[,t]/sum(P.normalized.tilde[,t])
    P.normalized.tilde[,t]=(1-alpha)*P.normalized.tilde[,t]+alpha/n
    
      #cumulative sum of "log returns" at each stock
    log.prod.X[,t]=log.prod.X[,t-1]+log(X[,t])
      #
    log.prod.EG.normalized.tilde.returns[t]=log.prod.EG.normalized.tilde.returns[t-1]+log(P.normalized.tilde[,t]%*%X[,t])
    
      #updated number of shares (in $) we own in each company
    money_portfolio[,t] <- buying_power * P.normalized.tilde[,t]
      #updated number of shares (in #) we own in each company
    share_portfolio[,t] <- money_portfolio[,t] / stocks[,t]
  }
  
  
    #sell entire portfolio at the end of the 10 years
  purse[p] <- sum(share_portfolio[,T] * stocks[,p]) - n*fee
  
    #RETURN ON INVESTMENT
  return.on.investment <- purse[p] - initial.investment
  return(return.on.investment)
}
#########################################################################################################

#testing with difference inputs
  #finance
EG.investment(5000, finance, fee=0, eta=150)
EG.investment(5000, finance, fee=0, eta=75)
EG.investment(5000, finance, fee=0, eta=25)
EG.investment(5000, finance, fee=0, eta=1)
EG.investment(5000, finance, fee=0, eta=.25)
  #health
EG.investment(25000, health, fee=4.95, eta=150)
EG.investment(25000, health, fee=4.95, eta=75)
EG.investment(25000, health, fee=4.95, eta=25)
EG.investment(25000, health, fee=4.95, eta=1)
EG.investment(25000, health, fee=4.95, eta=.25)
  #transportation
EG.investment(5000, trans, fee=0, eta=150)
EG.investment(5000, trans, fee=0, eta=75)
EG.investment(5000, trans, fee=0, eta=25)
EG.investment(5000, trans, fee=0, eta=1)
EG.investment(5000, trans, fee=0, eta=.25)
  #technology
EG.investment(5000, tech, fee=0, eta=150)
EG.investment(5000, tech, fee=0, eta=75)
EG.investment(5000, tech, fee=0, eta=25)
EG.investment(5000, tech, fee=0, eta=1)
EG.investment(5000, tech, fee=0, eta=.25)
  #energy
EG.investment(5000, energy, fee=0, eta=150)
EG.investment(5000, energy, fee=0, eta=75)
EG.investment(5000, energy, fee=0, eta=25)
EG.investment(5000, energy, fee=0, eta=1)
EG.investment(5000, energy, fee=0, eta=.25)
