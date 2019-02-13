#Modified EG.revised Eta Algorithm
#Edited - Nick Kurtansky - 4-21-2018 : 3:28 PM

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

EG.investment=function(initial.investment, matrix.of.stock.prices, fee=4.95, eta=sqrt(8*(alpha^2)*log(n)/((n^2)*T)), industry_name){
  
    #per-transaction brokerage fee

    #keep track of fortune at each time
  purse <- rep(NA, ncol(matrix.of.stock.prices))
  purse[1] <- initial.investment
  
    #keep track of BCRP fortune as well
  bcrp_purse <- rep(NA, ncol(matrix.of.stock.prices))
  bcrp_purse[1] <- initial.investment
  
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
  

    #BCRP
  rand.BCRP <- alg_BCRP(get_price_relatives(t(stocks)), method="rand", samplings=1000)
  BCRP.w <- as.vector(rand.BCRP$Weights[1,])
    #matrix for number shares (in $) with bcrp weights
  bcrp_money_portfolio <- matrix(NA, n, p)
  bcrp_money_portfolio[,1] <- (purse[1]-1*fee) * BCRP.w
    #initialize matrix of the number of shares (in #) we own in each company at each time
  bcrp_share_portfolio <- matrix(NA, n, p)
  bcrp_share_portfolio[,1] <- bcrp_money_portfolio[,1] / stocks[,1]
  
  
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
    #EG PART OF ITERATION t
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
    
    
    #BCRP PART OF ITERATION t
      #sell entire portfolio
    bcrp_purse[t] <- sum(bcrp_share_portfolio[,t-1] * stocks[,t]) - n*fee
    bcrp_buying_power <- bcrp_purse[t] - n*fee
    
      #updated number of shares (in $) we own in each company
    bcrp_money_portfolio[,t] <- bcrp_buying_power * BCRP.w
      #updated number of shares (in #) we own in each company
    bcrp_share_portfolio[,t] <- bcrp_money_portfolio[,t] / stocks[,t]
    
  }
  
  
    #sell entire EG portfolio at the end of the 10 years
  purse[p] <- sum(share_portfolio[,T] * stocks[,p]) - n*fee
  
    #sell entire BCRP portfolio at the end of the 10 years
  bcrp_purse[p] <- sum(bcrp_share_portfolio[,T] * stocks[,p]) - n*fee
  
  
  #print(P.normalized.tilde)
  
    #RETURN ON INVESTMENT
  return.on.investment <- purse[p] - initial.investment
  return(return.on.investment)
   
   #   PLOT RETURN ON INVESTMENT
   #   individual stocks (try plotting the individual stock growth over time)
   sit.and.wait <- as.data.frame(sit.and.wait)
   sit.and.wait <- t(sit.and.wait)
   colnames(sit.and.wait) <- row.names(matrix.of.stock.prices)
   
   #   # THIS IS THE PLOT
   # color.vecs <- c("slateblue4", "darkgoldenrod1", "steelblue", "steelblue1", "steelblue2", "thistle", "thistle1", "thistle2", "thistle3", "skyblue", "skyblue2", "skyblue3")
   # graph_title <- paste( "~ Industry: ", industry_name,  " / Investment: ", initial.investment, " / Per Transaction Charge: ", fee, " / Eta: ", eta, " ~")
   # png_file_name <- paste(industry_name, initial.investment, fee, eta, ".png", sep="_")
   # 
   # png(file=png_file_name,width=1000,height=650)
   # 
   # plot(1:120, purse-initial.investment,type="l", col=color.vecs[1], lwd=3, 
   #      ylab="", xlab="", ylim=c(min(sit.and.wait)-initial.investment, max(bcrp_purse)-initial.investment))
   # par(new=T)
   # lines(1:120, bcrp_purse - initial.investment, type="l", col=color.vecs[2], lwd=2)
   # par(new=T)
   # for(i in 1:10){lines(1:120, sit.and.wait[,i]-initial.investment, type="l", col=color.vecs[i+2])}
   # par(cex.main=1.25, cex.axis=1.25, cex.sub=1.25)
   # title(main=graph_title,
   #       xlab="Number of Months Since Initial Investment in January 2008", ylab="Return on Investment ($)",
   #       par.settings=list(par.main.text=list(cex=5)))
   # abline(h=0, lwd=.5)
   # 
   # dev.off()
   
   
}
#########################################################################################################

#testing with difference inputs
  #finance
EG.investment(50000, finance, fee=4.95, eta=.05, industry_name = "Finance")

  #health
EG.investment(50000, health, fee=4.95, eta=.05)

  #transportation
EG.investment(50000, trans, fee=4.95, eta=.05)

  #technology
EG.investment(50000, tech, fee=4.95, eta=.05)

  #energy
EG.investment(50000, energy, fee=4.95, eta=.05)


