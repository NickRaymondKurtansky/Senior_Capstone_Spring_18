#########################################################################################################
# Function implementing EG eta update algorithm after normalizing so max(X[,t])\[Congruent]1.
# Keyed to Helmbold, Schapire, et. al. paper.
# EG eta for growth of fortune in an evolving portfolio on n investments
# Goal: Approximately match max with respect to u of product u\[Bullet]X[,1] ... u\[Bullet]X[,T].
# This goal is best with hindsight of the earnings of all constant rebalanced portfolios.
# X is matrix of price relatives. We will automatically redefine X[i,1]=1 to begin with.

EG.revised=function(n.by.T.matrix.of.price.relatives){
  
    #re-name the input: x
  X=n.by.T.matrix.of.price.relatives
    #n = # of stocks
  n=length(X[,1])
    #T = # of transaction periods
  T=length(X[1,])
    #alpha
  alpha=((n^2)*log(n)/(8*T))^(1/4)
    #eta: adjustment parameter
  eta=sqrt(8*(alpha^2)*log(n)/((n^2)*T))
  
    #print the number of stocks and number of time periods
  print(c(n,T))
    #print alpha and eta
  print(c(alpha,eta))
  
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
  
    #number of iterations = 120-1 (b/c we've already initialized the first set of weights)
  for(t in 1:(T-1)){
      #determine the weights vector at each time
    P.normalized.tilde[,t+1]=P.normalized.tilde[,t]*exp(eta*X.normalized.tilde[,t]/(P.normalized.tilde[,t]%*%X.normalized.tilde[,t]))
    P.normalized.tilde[,t+1]=P.normalized.tilde[,t+1]/sum(P.normalized.tilde[,t+1])
    P.normalized.tilde[,t+1]=(1-alpha)*P.normalized.tilde[,t+1]+alpha/n
    
      #cumulative sum of "log returns" at each stock
    log.prod.X[,t+1]=log.prod.X[,t]+log(X[,t+1])
      #
    log.prod.EG.normalized.tilde.returns[t+1]=log.prod.EG.normalized.tilde.returns[t]+log(P.normalized.tilde[,t+1]%*%X[,t+1])
  }
  
    #
  print(log.prod.EG.normalized.tilde.returns[T]/T)
    #
  print(log.prod.X[,T]/T)
    #
  plot(1:T,log.prod.EG.normalized.tilde.returns/(1:T), type="l") 
  for(i in 1:n){lines(1:T,log.prod.X[i,]/(1:T),type="l",col="red")}  
} 
#########################################################################################################

