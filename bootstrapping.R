#Bootstrapping return on investment
#Edited - Nick Kurtansky - 4-20-2018 : 4:15 PM

#load any necessary libraries
library(dplyr)
library(ggplot2)
library(boot)


################################################################################################
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


#load the stock data we built specifically for the purpose of inputting into the algorithm
load('Inputs.RData')
finance <- t(Inputs$Finance)
health <- t(Inputs$Healthcare)
trans <- t(Inputs$Transportation)
tech <- t(Inputs$Technology)
energy <- t(Inputs$Energy)

industry.matrix <- list(finance, health, trans, tech, energy)
industry.names <- c("Finance", "Healthcare", "Transportation", "Technology","Energy")
etas <- c(.05, 1, 20, 60, 150)
etas.char <- as.character(etas)


#####################################################################
  #Option 2: bootstrap CI through all cases

  #initialize the matrix of bootsrapped confidence intervals for return on 10000 investment (no service charge)
Boot_CI_matrix <- matrix(NA, 5, 5)
colnames(Boot_CI_matrix) <- etas.char
rownames(Boot_CI_matrix) <- industry.names

  #calculate the bootstrap CI for each industry at each eta
for(i in 1:5){
  for(j in 1:5){
    theta.hat <- EG.investment(10000, industry.matrix[[i]], fee=0, eta=etas[j])
    B=500
    B.thetahat <- rep(NA, times=B)
    for(b in 1:B){
      #resample from original sample
      ids <- sample(1:10, size=10, replace=TRUE) 
      stock.prices.star <- industry.matrix[[i]][ids,]
      #return on investment from bootstrap sample b
      return.star <- EG.investment(10000, stock.prices.star, fee=0, eta=etas[j])
      #add bootstrap return b to the list of all bootstrap returns
      B.thetahat[b] <- return.star
    }
    se.thetahat <- sd(B.thetahat) 
    CI.vector <- as.vector((round(c(theta.hat - 1.96*se.thetahat, theta.hat + 1.96*se.thetahat), 2)))
    CI.string <- paste(CI.vector[1], ", ", CI.vector[2], sep="")
    Boot_CI_matrix[i,j] <- CI.string
  }
}

#####################################################################
  #Option1: Bootstrap CI for an individual case
theta.hat <- EG.investment(1000, energy, fee=0, eta=50)
B=500
B.thetahat <- rep(NA, times=B)
for(i in 1:B){
  #resample from original sample
  ids <- sample(1:10, size=10, replace=TRUE) 
  finance.star <- finance[ids,]
  #return on investment from bootstrap sample i
  return.star <- EG.investment(1000, finance.star, fee=0, eta=50)
  #add bootstrap return i to the list of all bootstrap returns
  B.thetahat[i] <- return.star
}
se.thetahat <- sd(B.thetahat) 
se.thetahat
round(c(theta.hat - 1.96*se.thetahat, theta.hat + 1.96*se.thetahat), 2)
