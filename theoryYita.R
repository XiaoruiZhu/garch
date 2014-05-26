# n is the error length
# dfest.G is the parameters' vector of quais likelihood with the same 
# time series that have t innovation
# dfest.t is the vector of quais likelihood parameters 
# dfsim is the simulation freedom of error distribution
# rn is the selection pool of error distribution
# 计算理论的e t分布看yita的情况，与模型中用样本残差来估计的yita进行对比


# 改YITAGQMLE中关于广义高斯的分布密度，需要改得和R中rpgnorm一样

theoryYita=function(n,dfsim.error,rn,dfest.G,dfest.t){
  nsim=length(dfsim.error)
  ng=length(dfest.G)
  nt=length(dfest.t)
  yi.g.e=matrix(NA,ng,nsim)
  yi.t.e=matrix(NA,nt,nsim)
  for (p in 1:nsim){
    e.t=rn(n,dfsim.error[p])
    for (i in 1:ng){
      yi.g.e[i,p]=YITAGQMLE(h=n,e=e.t,dfest=dfest.G[i])
    }
    for (j in 1:nt){
      yi.t.e[j,p]=YITAtQMLE(h=n,e=e.t,dfest=dfest.t[j])
    }
  }
  list(G.T=yi.g.e,T.T=yi.t.e)  
}
