# Fan 文章中的方法，两步NGQMLE。但是存在伊塔不出来的问题
NGQMLE=function(h,x,dfest,est.para){
  #第一步计算yita
  df=dfest
#   sim1my=MyMLE(h,x)
#   estpara=sim1my$mle.N
#   直接传递MLE估计参数进来，减少运算
  e.t=com.e(est.para,x=x)  
  yi.g.e=YITAtQMLE(h=h,e=e.t,df)
  #伊塔求出了传递yita近来直接用
#   yi.g.e=yita1
  # likelihood of NGQMLE in Garch model. PS: f() is the t distribution.
  NGQL=function(para){
    n=length(x)
    sig2=numeric(n);tem1=numeric(1);tem2=numeric(1)
    w=para[1]
    alpha=para[2]
    beta=para[3]
    sig2[1]=w/(1-alpha-beta)
    for (t in 2:n){
      tem2=beta*(tem2+tem1)
      tem1=x[t-1]^2
      sig2[t]=(w/(1-beta)+alpha*(tem1+tem2))
    }
    tQMLE.e=x/(yi.g.e*sqrt(sig2))
#     students't t distribution
    f=(gamma((df+1)/2)/(pi*df)^0.5/gamma(df/2))*(1+tQMLE.e^2/(df))^(-(df+1)/2)
    tQL=sum(-log(sqrt(sig2))+log(f))
    return(-tQL)
  }
  
  # t拟似然估计NGQMLE的第二步  likelihood function
  ngqmle=nlminb(c(0.01,0.01,0.01),NGQL,lower=c(0,0,0),upper=Inf)
  return(c(ngqmle$par,yi.g.e))
#   list(NGQMLE=ngqmle$par,Yita=yi.g.e)
}