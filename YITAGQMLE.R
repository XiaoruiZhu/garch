# h is the number of samples
# e is the innovation caculated by estimate parameters
# dfest is the parameter of Quais likelihood
# x is time series
YITAGQMLE=function(h,e,dfest){
  # GGD新息的GARCH(1,1) likelihood function
  #df=dfest
  like=function(e,df){
    yitaGL=function(par){
      yi=par[1]
      #GQMLE.e=e/yi
      if (yi>0) 
        {
          #     lam=(2^(-2/df)*gamma(1/df)/gamma(3/df))^0.5
          #     f=df/(lam*2^(1+1/df)*gamma(1/df))*exp(-0.5*(abs(GQMLE.e/lam))^df)
          #     用p-generalized gaussian distribution
          f=df^(1-1/df)/2/gamma(1/df)*exp(-((abs(e/yi))^df)/df)
          sum(log(yi)-log(f))/h
        }
      else Inf
    }
    yitaGL
  }
  yigqmle=nlminb(c(0.01),like(e,df=dfest),lower=c(0),upper=Inf)
  return(yigqmle$par)
}
