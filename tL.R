# t分布新息的GARCH(1,1) likelihood function
tL=function(para){
  n=length(x)
  sig=numeric(n);tem1=numeric(1);tem2=numeric(1)
  w=para[1]
  alpha=para[2]
  beta=para[3]
  sig[1]=w/(1-alpha-beta)
  for (t in 2:n){
    tem1=tem1+beta^(t-2)
    tem2=beta*(tem2)+x[t-1]^2
    sig[t]=w*sig[1]+alpha*(tem2)+beta^(t-1)*sig[1]
  }
  tL=-1/n*sum(log(sig^0.5)+x^2/(2*(sig)))
  return(-tL)
}