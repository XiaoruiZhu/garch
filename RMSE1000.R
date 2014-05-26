# 计算RMSE的制表
RMSE1000=function(dat,a,b){
  a1=a;b1=b
  # 到了下限才估计出来的不删除来计算RMSE
  NEW.2NGQ3.1000=dat$E.2NGQMLE
  NEW.AtQ3.1000=dat$E.AtQ
  NEW.GQ3.1000=dat$E.GQ
  NEW.tQMLE3.1000=dat$E.tQMLE
  len=length(NEW.AtQ3.1000[,4])
  avgdf=mean(NEW.AtQ3.1000[,4])
  # 计算2NQMLE的三个参数的RMSE
  mean(NEW.2NGQ3.1000)
  RMSE.2NGQ.w3=sqrt(sum((NEW.2NGQ3.1000[,1]-a1[1])^2)/len)
  RMSE.2NGQ.a3=sqrt(sum((NEW.2NGQ3.1000[,2]-a1[2])^2)/len)
  RMSE.2NGQ.b3=sqrt(sum((NEW.2NGQ3.1000[,3]-b1[1])^2)/len)
  # 计算adaptive tQMLE的三个参数的RMSE
  mean(NEW.AtQ3.1000[,4])
  RMSE.w3=sqrt(sum((NEW.AtQ3.1000[,1]-a1[1])^2)/len)
  RMSE.a3=sqrt(sum((NEW.AtQ3.1000[,2]-a1[2])^2)/len)
  RMSE.b3=sqrt(sum((NEW.AtQ3.1000[,3]-b1[1])^2)/len)
  # 计算GQ的RMSE
  # GQMLE的参数RMSE
  RMSE.GQ.w3=sqrt(sum((NEW.GQ3.1000[,1]-a1[1])^2)/len)
  RMSE.GQ.a3=sqrt(sum((NEW.GQ3.1000[,2]-a1[2])^2)/len)
  RMSE.GQ.b3=sqrt(sum((NEW.GQ3.1000[,3]-b1[1])^2)/len)
  # tQMLE的参数RMSE
  RMSE.tQ.w3=sqrt(sum((NEW.tQMLE3.1000[,1]-a1[1])^2)/len)
  RMSE.tQ.a3=sqrt(sum((NEW.tQMLE3.1000[,2]-a1[2])^2)/len)
  RMSE.tQ.b3=sqrt(sum((NEW.tQMLE3.1000[,3]-b1[1])^2)/len)
  ## 计算相对于GQ的比值
  ratio.2NGQ=c(RMSE.2NGQ.w3,RMSE.2NGQ.a3,RMSE.2NGQ.b3)/c(RMSE.GQ.w3,RMSE.GQ.a3,RMSE.GQ.b3)
  ratio.AtQ=c(RMSE.w3,RMSE.a3,RMSE.b3)/c(RMSE.GQ.w3,RMSE.GQ.a3,RMSE.GQ.b3)
  ratio.tQ=c(RMSE.tQ.w3,RMSE.tQ.a3,RMSE.tQ.b3)/c(RMSE.GQ.w3,RMSE.GQ.a3,RMSE.GQ.b3)
  RMSE.GQ=c(RMSE.GQ.w3,RMSE.GQ.a3,RMSE.GQ.b3)
  tableRMSE=rbind(avgdf,ratio.AtQ,ratio.2NGQ,RMSE.GQ,ratio.tQ)
  return(tableRMSE)
  ## 结果按照( 平均bestdf AtQ比例 2NGQ比例 RMSEofGQ tQ比例 )来排列
}
