# likelihood of NGQMLE in Garch model. PS: f() is the t distribution.
NGQL=function(para){
  n=length(x)
  sig=numeric(n);tem1=numeric(1);tem2=numeric(1)
  w=para[1]
  alpha=para[2]
  beta=para[3]
  sig[1]=w/(1-alpha-beta)
  for (t in 2:n){
    tem1=tem1+beta^(t-2)
    tem2=beta*(tem2)+x[t-1]^2
    sig[t]=w/(1-beta)+alpha*(tem2)+beta^(t-1)*sig[1]
  }
  tQMLE.e=x/(yi.g.e*sqrt(sig))
  f=(gamma((df+1)/2)/(pi*df)^0.5/gamma(df/2))*(df/(df-2))^0.5*(1+tQMLE.e^2/(df-2))^(-(df+1)/2)
  tQL=sum(-log(sig^0.5)+log(f))
  return(-tQL)
}
