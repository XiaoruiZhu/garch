# 计算RMSE的制表
RMSEemitNA=function(dat,a,b){
  a1=a;b1=b
  # 需要将没估计出来的删除来计算RMSE
  NApara3=which(dat$E.AtQ[,4]==0.5)
  length(NApara3)
  NEW.AtQ3.1000=dat$E.AtQ[-NApara3,]
  NEW.GQ3.1000=dat$E.GQ[-NApara3,]
  NEW.tQMLE3.1000=dat$E.tQMLE[-NApara3,]
  # 计算adaptive tQMLE的三个参数的RMSE
  mean(NEW.AtQ3.1000[,4])
  RMSE.w3=sum((NEW.AtQ3.1000[,1]-a1[1])^2)/length(NEW.AtQ3.1000[,4])
  RMSE.a3=sum((NEW.AtQ3.1000[,2]-a1[2])^2)/length(NEW.AtQ3.1000[,4])
  RMSE.b3=sum((NEW.AtQ3.1000[,3]-b1[1])^2)/length(NEW.AtQ3.1000[,4])
  # 计算GQ的RMSE
  # GQMLE的参数RMSE
  RMSE.GQ.w3=sum((NEW.GQ3.1000[,1]-a1[1])^2)/length(NEW.AtQ3.1000[,4])
  RMSE.GQ.a3=sum((NEW.GQ3.1000[,2]-a1[2])^2)/length(NEW.AtQ3.1000[,4])
  RMSE.GQ.b3=sum((NEW.GQ3.1000[,3]-b1[1])^2)/length(NEW.AtQ3.1000[,4])
  # tQMLE的参数RMSE
  RMSE.tQ.w3=sum((NEW.tQMLE3.1000[,1]-a1[1])^2)/length(NEW.AtQ3.1000[,4])
  RMSE.tQ.a3=sum((NEW.tQMLE3.1000[,2]-a1[2])^2)/length(NEW.AtQ3.1000[,4])
  RMSE.tQ.b3=sum((NEW.tQMLE3.1000[,3]-b1[1])^2)/length(NEW.AtQ3.1000[,4])
  
  RMSE3.1000=rbind(c(RMSE.w3,RMSE.a3,RMSE.b3),
                   c(RMSE.GQ.w3,RMSE.GQ.a3,RMSE.GQ.b3),c(RMSE.tQ.w3,RMSE.tQ.a3,RMSE.tQ.b3))
  return(RMSE3.1000)
  
}
