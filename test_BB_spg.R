#调试
lik<-function(x){
  TTL=function(par){
    n=length(x)
    sig2=numeric(n);tem1=numeric(1);tem2=numeric(1)
    w=par[1]
    alpha=par[2]
    beta=par[3]
    sig2[1]=w/(1-alpha-beta)
    for (t in 2:n){
      tem2=beta*(tem2+tem1)
      tem1=x[t-1]^2
      sig2[t]=(w/(1-beta)+alpha*(tem1+tem2)) #这按照笔记红色的通项公式改写可计算GARCH(P,Q)  
    }
    if (c(w,alpha,beta) > rep(0,3) && (alpha+beta) < 1 && sig2>0)  
    {
      sum(log(sqrt(sig2))+x^2/(sig2))
    }
        else Inf
  }
    #     LL=-sum(log(sqrt(sig2))+x^2/(2*sig2))
  TTL
}


#plot(c(1:500),x,type='l')
mle.N=nlminb(c(0.1,0.1,0.1),lik(x=return_SP),lower=c(0,0,0),upper=c(Inf,1,1))
mle.N