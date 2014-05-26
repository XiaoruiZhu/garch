# 厚尾GARCH模型的nlminb与TSA估计结果随样本量增大效果
# The function draw pictures of different results of MYMLE and TSA.garch with the 
# sample number increasion.
# maxs is the biggest number of simulation.
# gap is of each simulation
# repN is the repeat number 
# a is alpha include the incept and the parameter of autoregression parts
# b is a vector include the parameter of conditional variance
MSEbysam=function(maxs,gap,repN,a,b){
  Hmynlmsdall=matrix(c(a,b),nrow=1,ncol=3,dimnames=list(c('Hmynlm.all'),c('w','a','b')))
  Htsasdall=matrix(c(a,b),nrow=1,ncol=3,dimnames=list(c('HTSA.all'),c('w','a','b')))
  for (j in 1:(maxs/gap)){
    Htsa=matrix(c(a,b),nrow=1,ncol=3,dimnames=list(c('HTSA'),c('w','a','b')))
    Hmynlm=matrix(c(a,b),nrow=1,ncol=3,dimnames=list(c('Hmynlm'),c('w','a','b')))
    for (i in 1:repN){
      tem=duibi(gap*j,a,b)$com.para
      Hmynlm=rbind(Hmynlm,(tem[3,]-c(a,b))^2)
      Htsa=rbind(Htsa,(tem[2,]-c(a,b))^2)
    }
    Htsasd=(apply(Htsa[-1,],2,mean))^0.5
    Htsasdall=rbind(Htsasdall,Htsasd)
    Hmynlmsd=(apply(Hmynlm[-1,],2,mean)^0.5)
    Hmynlmsdall=rbind(Hmynlmsdall,Hmynlmsd)
  }
  numsim=matrix(gap*c(1:(maxs/gap)),nrow=maxs/gap,ncol=1)
  Htsasdall=cbind(numsim,Htsasdall[-1,])
  Hmynlmsdall=cbind(numsim,Hmynlmsdall[-1,])
  list(RMSE.TSA=Htsasdall,RMSE.MLE=Hmynlmsdall)

}
