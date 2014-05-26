a1=c(0.02,0.6);b1=0.3
# a2=c(0.02,0.05);b2=0.9

# repn is the simulation times of each type. Such as, t(3),250 number sample repeat repn times
# samples is the length of time-series
# dfsim is the innovation distribution
# AtQ is the adaptive t-QMLE
# GQ is the ordinary MLE with the Normal distribution error.
# tMLE is the tQMLE with the specified true distribution, in simulation study t(3),t(4)..
finalsim=function(repn,samples,dfsim){
  AtQ=matrix(NA,repn,4)
  GQ=matrix(NA,repn,3)
  tQMLE=matrix(NA,repn,3)
  for (i in 1:repn){
    x=T.GARCH(alpha=a1,beta=b1,rnd=rt,df.t=dfsim,n=samples)
    AtQ[i,]=A_tQMLE(samples,x)
    GQ[i,]=MyMLE(samples,x)$mle.N
    tQMLE[i,]=MytQMLE(samples,x,dfest=dfsim)$qmle.N   
  }
  list(E.AtQ=AtQ,E.GQ=GQ,E.tQMLE=tQMLE)
}
# 重点的估计
t3.250=finalsim(repn=20,samples=250,dfsim=3)
t3.500=finalsim(repn=500,samples=500,dfsim=3)
t3.1000=finalsim(repn=500,samples=1000,dfsim=3)

# 需要的空的每估计出来的删除来计算RMSE

ls(t3.250)
source('RMSE1000.R')
a1=c(0.5,0.55);b1=0.4
# which(t3.250$E.AtQ[,4]==0.3)
RMSE1000(dat=t4.500,a=a1,b=b1)
t3.250$E.AtQ[1:4,]
t3.250$E.GQ[1:4,]
NApara=which(t3.250$E.AtQ[,4]==0.3)
length(NApara)

NEW.AtQ3.250=t3.250$E.AtQ[-NApara,]
NEW.GQ3.250=t3.250$E.GQ[-NApara,]
NEW.tQMLE3.250=t3.250$E.tQMLE[-NApara,]
length(NEW.GQ3.250[,1])
# 计算三个参数的RMSE
mean(NEW.AtQ3.250[,4])
# Adaptive tQMLE的参数RMSE
RMSE.w1=sum((NEW.AtQ3.250[,1]-a1[1])^2)/length(NEW.AtQ3.250[,4])
RMSE.a1=sum((NEW.AtQ3.250[,2]-a1[2])^2)/length(NEW.AtQ3.250[,4])
RMSE.b1=sum((NEW.AtQ3.250[,3]-b1[1])^2)/length(NEW.AtQ3.250[,4])
# GQMLE的参数RMSE
RMSE.GQ.w1=sum((NEW.GQ3.250[,1]-a1[1])^2)/length(NEW.AtQ3.250[,4])
RMSE.GQ.a1=sum((NEW.GQ3.250[,2]-a1[2])^2)/length(NEW.AtQ3.250[,4])
RMSE.GQ.b1=sum((NEW.GQ3.250[,3]-b1[1])^2)/length(NEW.AtQ3.250[,4])
# tQMLE的参数RMSE
RMSE.tQ.w1=sum((NEW.tQMLE3.250[,1]-a1[1])^2)/length(NEW.AtQ3.250[,4])
RMSE.tQ.a1=sum((NEW.tQMLE3.250[,2]-a1[2])^2)/length(NEW.AtQ3.250[,4])
RMSE.tQ.b1=sum((NEW.tQMLE3.250[,3]-b1[1])^2)/length(NEW.AtQ3.250[,4])

RMSE3.250=rbind(c(RMSE.w1,RMSE.a1,RMSE.b1),
                c(RMSE.GQ.w1,RMSE.GQ.a1,RMSE.GQ.b1),c(RMSE.tQ.w1,RMSE.tQ.a1,RMSE.tQ.b1))
round(RMSE3.250,digits=4)

# t3.500的模拟，已经做完，制表？
t3.500$E.AtQ[13:22,]
t3.500$E.GQ[13:22,]
t3.500$E.tQMLE[13:22,]
# 需要将没估计出来的删除来计算RMSE
NApara2=which(t3.500$E.AtQ[,4]==0.3)
length(NApara2)
NEW.AtQ3.500=t3.500$E.AtQ[-NApara2,]
NEW.GQ3.500=t3.500$E.GQ[-NApara2,]
NEW.tQMLE3.500=t3.500$E.tQMLE[-NApara2,]
length(NEW.GQ3.500[,3])
# 计算adaptive tQMLE的三个参数的RMSE
mean(NEW.AtQ3.500[,4])
RMSE.w2=sum((NEW.AtQ3.500[,1]-a1[1])^2)/length(NEW.AtQ3.500[,4])
RMSE.a2=sum((NEW.AtQ3.500[,2]-a1[2])^2)/length(NEW.AtQ3.500[,4])
RMSE.b2=sum((NEW.AtQ3.500[,3]-b1[1])^2)/length(NEW.AtQ3.500[,4])
# 计算GQ的RMSE
# GQMLE的参数RMSE
RMSE.GQ.w2=sum((NEW.GQ3.500[,1]-a1[1])^2)/length(NEW.AtQ3.500[,4])
RMSE.GQ.a2=sum((NEW.GQ3.500[,2]-a1[2])^2)/length(NEW.AtQ3.500[,4])
RMSE.GQ.b2=sum((NEW.GQ3.500[,3]-b1[1])^2)/length(NEW.AtQ3.500[,4])
# tQMLE的参数RMSE
RMSE.tQ.w2=sum((NEW.tQMLE3.500[,1]-a1[1])^2)/length(NEW.AtQ3.500[,4])
RMSE.tQ.a2=sum((NEW.tQMLE3.500[,2]-a1[2])^2)/length(NEW.AtQ3.500[,4])
RMSE.tQ.b2=sum((NEW.tQMLE3.500[,3]-b1[1])^2)/length(NEW.AtQ3.500[,4])

RMSE3.500=rbind(c(RMSE.w2,RMSE.a2,RMSE.b2),
                c(RMSE.GQ.w2,RMSE.GQ.a2,RMSE.GQ.b2),c(RMSE.tQ.w2,RMSE.tQ.a2,RMSE.tQ.b2))
round(RMSE3.500,digits=4)

# t3.1000的制表对比！！
t3.1000$E.AtQ[13:22,]
t3.1000$E.GQ[13:22,]
t3.1000$E.tQMLE[13:22,]
# 需要将没估计出来的删除来计算RMSE
NApara3=which(t3.1000$E.AtQ[,4]==0.3)
NApara3
length(NApara3)
NEW.AtQ3.1000=t3.1000$E.AtQ[-NApara3,]
NEW.GQ3.1000=t3.1000$E.GQ[-NApara3,]
NEW.tQMLE3.1000=t3.1000$E.tQMLE[-NApara3,]
length(NEW.GQ3.1000[,3])
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
round(RMSE3.1000,digits=4)

# 需要将没估计出来的删除来计算RMSE


######################后面还未模拟#########
t3.2000=finalsim(repn=1000,samples=2000,dfsim=3)
save(t3.2000, file = "Y:\\study!!\\论文编辑与成型\\GARCH\\simulation\\t3_2000.RData")
rm(t3.2000)

t4.250=finalsim(repn=1000,samples=250,dfsim=4)
save(t4.250, file = "Y:\\study!!\\论文编辑与成型\\GARCH\\simulation\\t4_250.RData")
rm(t4.250)

t4.500=finalsim(repn=1000,samples=500,dfsim=4)
save(t4.500, file = "Y:\\study!!\\论文编辑与成型\\GARCH\\simulation\\t4_500.RData")
rm(t4.500)

t4.1000=finalsim(repn=1000,samples=1000,dfsim=4)
save(t4.1000, file = "Y:\\study!!\\论文编辑与成型\\GARCH\\simulation\\t4.1000.RData")
rm(t4.1000)

t6.250=finalsim(repn=1000,samples=2500,dfsim=6)
save(t6.250, file = "Y:\\study!!\\论文编辑与成型\\GARCH\\simulation\\t6.250.RData")
rm(t6.250)

t6.500=finalsim(repn=1000,samples=500,dfsim=6)
save(t6.500, file = "Y:\\study!!\\论文编辑与成型\\GARCH\\simulation\\t6.500.RData")
rm(t6.500)

t6.1000=finalsim(repn=1000,samples=1000,dfsim=6)
save(t6.1000, file = "Y:\\study!!\\论文编辑与成型\\GARCH\\simulation\\t6.1000.RData")
rm(t6.1000)

t6.2000=finalsim(repn=1000,samples=2000,dfsim=6)
save(t6.2000, file = "Y:\\study!!\\论文编辑与成型\\GARCH\\simulation\\t6.2000.RData")
rm(t6.2000)
