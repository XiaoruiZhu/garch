
####普通的GARCH(1,1)似然函数likelihood
TTL=function(para){
  n=length(x)
  sig=numeric(n);tem1=numeric(1);tem2=numeric(1)
  w=para[1]
  alpha=para[2]
  beta=para[3]
  sig[1]=1/(1-alpha-beta)
  for (t in 2:n){
    tem1=tem1+beta^(t-2)
    tem2=beta*(tem2)+x[t-1]^2  
    sig[t]=w*tem1+alpha*(tem2)+beta^(t-1)*sig[1]
    #sig[t]=tem1+alpha*(tem2)+beta^(t-1)*sig[1]
  }
  LL=-1/T*sum(log(sqrt(sig))+x^2/(sig))###根据论文修改了一下
 # LL=-1/T*sum(log(w*sqrt(sig))+x^2/(2*w^2*sig))###根据论文修改了一下
  return(-LL)
}