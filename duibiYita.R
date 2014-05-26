############# 对比一次与bootstrap bootN次的伊塔！！#####
# bootstrap method of compute yita. It means bootstrap samples from the sample e with bootN times.
# dfest.G is the parameters' vector of quais likelihood with the same 
# time series that have t innovation
# dfest.t is the vector of quais likelihood parameters 
# bootN is the number of bootstrap. Resampling bootN times from the sample e. 
#   Resampling number of samples each time is equal to the length of time series. 
duibiYita=function(h,a,b,dfsim,dfest.G,dfest.t,bootN){
  t=T.GARCH(alpha=a,beta=b,rnd=rt,df.t=dfsim,n=h)
  sim1my=MyMLE(h,x=t)
  estpara=sim1my$mle.N
  ng=length(dfest.G)
  nt=length(dfest.t)
  e.t=com.e(estpara,x=t)
#   普通的伊塔计算，仅计算一次的时候
  yi.g.e=rep(NA,ng)
  for (i in 1:ng){
    yi.g.e[i]=YITAGQMLE(h=h,e=e.t,dfest=dfest.G[i])
  }
  yi.t.e=rep(NA,nt)
  for (j in 1:nt){
    yi.t.e[j]=YITAtQMLE(h=h,e=e.t,dfest=dfest.t[j])
  }
#   bootstrap的方式计算bootN次的求均值
  BSyi.g.e=matrix(NA,bootN,ng)
  BSyi.t.e=matrix(NA,bootN,nt)
  for (m in 1:bootN){
    BS.e.t=sample(e.t,replace=TRUE)
    for (i in 1:ng){
      BSyi.g.e[m,i]=YITAGQMLE(h=h,e=BS.e.t,dfest=dfest.G[i])
    }
    for (j in 1:nt){
      BSyi.t.e[m,j]=YITAtQMLE(h=h,e=BS.e.t,dfest=dfest.t[j])
    }
  }
  BSyi.g=apply(BSyi.g.e,2,mean)
  BSyi.t=apply(BSyi.t.e,2,mean)
  list(G.T=yi.g.e,T.T=yi.t.e,BSyi.G=BSyi.g,BSyi.t=BSyi.t)  
}
