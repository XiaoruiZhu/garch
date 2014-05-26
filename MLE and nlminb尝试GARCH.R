library(TSA)
x=garch.sim(alpha=c(0.05,0.06),beta=.3,n=10000000)
M.N=garch(x,order=c(1,1))
M.N
####普通的GARCH(1,1)似然函数likelihood
TTL=function(para){
  n=length(x)
  sig=numeric(n);sigsum=numeric(1)
  sig[1]=para[1]
  alpha=para[2]
  beta=para[3]
  for (t in 2:n){
    sigsum=sigsum+(beta^(t-2)*x[t-1]^2)
    sig[t]=sig[1]+alpha*(sigsum)
  }
  LL=-1/2*sum(log(sig)+x^2/(sig))
  return(-LL)
}

set.seed(10000)
x=garch.sim(alpha=c(0.5,0.6),beta=.3,n=100000)
mle.N=nlminb(c(0.1,0.1,0.1),TTL,lower=-Inf,upper=Inf)
mle.N
