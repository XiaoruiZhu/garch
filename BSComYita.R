# bootstrap method of compute yita. It means bootstrap samples from the sample e with bootN times.
# dfest.G is the parameters' vector of quais likelihood with the same 
# time series that have t innovation
# dfest.t is the vector of quais likelihood parameters 
# bootN is the number of bootstrap. Resampling bootN times from the sample e. 
#   Resampling number of samples each time is equal to the length of time series. 

BSComYita=function(x,dfest.G,dfest.t,bootN){
  t=x
  sim1my=MyMLE(h,x=t)
  estpara=sim1my$mle.N
  ng=length(dfest.G)
  nt=length(dfest.t)
  e.t=com.e(estpara,x=t)
  yi.g.e=matrix(NA,bootN,ng)
  yi.t.e=matrix(NA,bootN,nt)
  for (m in 1:bootN){
    BS.e.t=sample(e.t,replace=TRUE)
    for (i in 1:ng){
      yi.g.e[m,i]=YITAGQMLE(h=h,e=BS.e.t,dfest=dfest.G[i])
    }
    for (j in 1:nt){
      yi.t.e[m,j]=YITAtQMLE(h=h,e=BS.e.t,dfest=dfest.t[j])
    }
  }
  yi.g=apply(yi.g.e,2,mean)
  yi.t=apply(yi.t.e,2,mean)
  list(G.T=yi.g,T.T=yi.t)  
}
