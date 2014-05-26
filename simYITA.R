# # 计算yita进行对比，针对不同的error distribution对比不同的quasi likelihood
# n is the length of time series
# a is the alpha and parameters
# b is the beta parameters
# dfsim is the degree of freedom of error distribution, the default distribution is student's t 

simYITA=function(n,a,b,dfsim){
  ### 计算两步NGQMLE，Fan的方法
  h=n;a1=a;b1=b
  gc=c(1.5,2,2.5,3,4,6)
  tc=c(1,2,4,6,9,18,20)
  simG.T=rep(NA,6)
  simT.T=rep(NA,7)
  # 模拟innovation is student's t(dfsim) distribution
  x=T.GARCH(alpha=a1,beta=b1,rnd=rt,df.t=dfsim,n=h)
  simG.T=comyita(x,dfest.G=gc,dfest.t=tc)$G.T
  est1=comyita(x,dfest.G=gc,dfest.t=tc)$est
  simT.T=comyita(x,dfest.G=gc,dfest.t=tc)$T.T
  names(simG.T)=c(1.5,2,2.5,3,4,6)
  names(simT.T)=c(1,2,4,6,9,18,20)
  list(est1=est1,E.df=dfsim,G.T=simG.T,T.T=simT.T)
}