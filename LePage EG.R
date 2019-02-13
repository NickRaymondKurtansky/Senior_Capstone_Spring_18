# Function implementing EG eta update algorithm after normalizing so max(X[,t])\[Congruent]1.
# Keyed to Helmbold, Schapire, et. al. paper.
# EG eta for growth of fortune in an evolving portfolio on n investments
# Goal: Approximately match max with respect to u of product u\[Bullet]X[,1] ... u\[Bullet]X[,T].
# This goal is best with hindsight of the earnings of all constant rebalanced portfolios.
# X is matrix of price relatives. We will automatically redefine X[i,1]=1 to begin with.
EG.revised=function(n.by.T.matrix.of.price.relatives){
  X=n.by.T.matrix.of.price.relatives
  n=length(X[,1])
  T=length(X[1,])
  alpha=((n^2)*log(n)/(8*T))^(1/4)
  eta=sqrt(8*(alpha^2)*log(n)/((n^2)*T))
  print(c(n,T))
  print(c(alpha,eta))
  m=apply(X,2,max)
  normalize=function(v){v/max(v)}
  X.normalized=apply(X,2,normalize)
  X.normalized.tilde=(1-alpha/n)*X.normalized+(alpha/n)
  log.prod.X=log(X)
  log.prod.EG.normalized.tilde.returns=rep(0,T)
  P.normalized.tilde=X
  P.normalized.tilde[,1]=rep(1/n,n)
  for(t in 1:(T-1)){
    P.normalized.tilde[,t+1]=P.normalized.tilde[,t]*exp(eta*X.normalized.tilde[,t]/(P.normalized.tilde[,t]%*%X.normalized.tilde[,t]))
    P.normalized.tilde[,t+1]=P.normalized.tilde[,t+1]/sum(P.normalized.tilde[,t+1])
    P.normalized.tilde[,t+1]=(1-alpha)*P.normalized.tilde[,t+1]+alpha/n
    log.prod.X[,t+1]=log.prod.X[,t]+log(X[,t+1])
    log.prod.EG.normalized.tilde.returns[t+1]=log.prod.EG.normalized.tilde.returns[t]+log(P.normalized.tilde[,t+1]%*%X[,t+1])
  }
  print(P.normalized.tilde)
  
  print(log.prod.EG.normalized.tilde.returns[T]/T)
  print(log.prod.X[,T]/T)
  plot(1:T,log.prod.EG.normalized.tilde.returns/(1:T), type="l") 
  for(i in 1:n){lines(1:T,log.prod.X[i,]/(1:T),type="l",col="red")}  } 

Xa1=runif(200)
Xa2=1-Xa1^0.8
Xa3=.5*Xa1+.5*Xa2
Xa=rbind(Xa1,Xa2,Xa3)*300 
EG.revised(Xa)