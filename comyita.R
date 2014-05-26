# dfest.G is the parameters' vector of quais likelihood with the same 
# time series that have t innovation
# dfest.t is the vector of quais likelihood parameters 

comyita=function(x,dfsim,dfest.G,dfest.t){
  t=x
  sim1my=MyMLE(h,x=t)
  estpara=sim1my$mle.N
  ng=length(dfest.G)
  nt=length(dfest.t)
#  sim1my=xitong(h,a,b,x=t)
#  estpara=sim1my$coef
  e.t=com.e(estpara,x=t)
  source("YITAGQMLE.R")
  yi.g.e=rep(NA,ng)
  for (i in 1:ng){
    yi.g.e[i]=YITAGQMLE(h=h,e=e.t,dfest=dfest.G[i])
  }
  source("YITAtQMLE.R")
  yi.t.e=rep(NA,nt)
  for (j in 1:nt){
    yi.t.e[j]=YITAtQMLE(h=h,e=e.t,dfest=dfest.t[j])
  }
  list(G.T=yi.g.e,T.T=yi.t.e,est=estpara)  
}
