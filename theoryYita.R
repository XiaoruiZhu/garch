# n is the error length
# dfest.G is the parameters' vector of quais likelihood with the same 
# time series that have t innovation
# dfest.t is the vector of quais likelihood parameters 
# dfsim is the simulation freedom of error distribution
# rn is the selection pool of error distribution
# �������۵�e t�ֲ���yita���������ģ�����������в������Ƶ�yita���жԱ�


# ��YITAGQMLE�й��ڹ����˹�ķֲ��ܶȣ���Ҫ�ĵú�R��rpgnormһ��

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