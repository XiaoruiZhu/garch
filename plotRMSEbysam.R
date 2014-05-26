plot.RMSEbysam=function(da.tsa,da.mle){
  Htsasdall=da.tsa
  Hmynlmsdall=da.mle
  ## w做对比图
  plot(Hmynlmsdall[,1],Hmynlmsdall[,2],
       main="RMSE of w by the 'nlminb' algorithm",xlab="sample size", 
       ylab="estimator of w",type='l',col=4)
  plot(Htsasdall[,1],Htsasdall[,2],
       main="RMSE of w by the 'TSA' package",xlab="sample size", 
       ylab="estimator of w",type='l',col=4)
  ## alpha做对比图
  plot(Hmynlmsdall[,1],Hmynlmsdall[,3],
       main="RMSE of alpha under the 'nlminb' algorithm",
       xlab="sample size", ylab="estimator of alpha",type='l',col=4)
  plot(Htsasdall[,1],Htsasdall[,2],
       main="RMSE of alpha by the 'TSA' package",xlab="sample size", 
       ylab="estimator of w",type='l',col=4)  
  ## beta作图对比
  plot(Hmynlmsdall[,1],Hmynlmsdall[,4],
       main="RMSE of beta by the 'nlminb' algorithm",
       xlab="sample size", ylab="estimator of beta",type='l',col=4)
  plot(Htsasdall[,1],Htsasdall[,2],
       main="RMSE of beta by the 'TSA' package",xlab="sample size", 
       ylab="estimator of w",type='l',col=4)}
