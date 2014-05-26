###------------------ǰ�ڰ��������-------------------------------------
#ϵͳ��TSA������������ͨ��������ΪN��GARCH����
library(TSA)
library(tseries)
#  T�ֲ���GARCH(1,1)������ݵ����ɣ�ֻ����t���������GARCH(1,1)
source("T.GARCH.R")
##�Ѿ��ҵ���pgnorm�����rpgnorm����
#Function to generate garch model with generalized Gaussian innovation
source("G.GARCH.R")

#��ͨ��GARCH(1,1)��Ȼ����likelihood
#   source("TTL.R")
#GASSAIN�ֲ���Ϣ��GARCH��likelihood
#   source("tL.R")
#��˹����ȻGARCH(1,1)��Ȼ����likelihood
# �Ѿ��ҵ���������rpgnorm�ǲ����ģ��Ѿ�ͳһ�����º�R���еķֲ�pdf
#�ҵ���ͨ����̬GARCH(1,1)��MLE���Ʒ���
source("MyMLE.R")                                  ###����������Ҫ����淶������garch(p,q)
source("MytQMLE.R")
source("MyGQMLE.R")
# two-stage NGQMLE with yita scare parameter
# likelihood function of NGQMLE by using t distribution
source('NGQL.R')
# NGQMLE procedure in Fan's paper
source("NGQMLE.R")
#ϵͳTSA���Դ���garch()���ƺ������й��ư�
source("xitong.R")
#�ԱȽ������
#�Ա�normal distribution innovation��garch�Ĺ���MLE��MYMLE
source("duibi.R")
#���������в��
#  �����еĲв����У�����ԭʼֵ/�������
source("com.e.R")
# ��˹����Ȼ���Ƽ����ĶԱ�
#  ���㹤�߱�����
source("YITAGQMLE.R")
source("YITAtQMLE.R")
# compute the yita with varies Generalized Gaussian and t
source('comyita.R')
# insert the bootstrap resampling into the procedure which caculate the estimate error
source('BSComYita.R')
# ģ�����ݱ仯ʱ���߱����ĶԱȣ�����comyita.r
source('simYITA.R')
# ��������yita�ı仯��������Ϣ�ֲ���ͬ��qmle����yita�õ�likelihood��ͬ
source('theoryYita.R')

# compare the NGQMLE with the MLE and the GQMLE.
source("NTGduibi.R")
# Adaptive t distribution quasi-maximum likelihood function
source("EstmBestdf.R")
source("A_tQMLE.R")

source('RMSE1000.R')

source('RMSEemitNA.R')
# !!!!!!!

# ���벿��END---------------------------------------------------------------------


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
  NGQ=matrix(NA,repn,4)
  for (i in 1:repn){
    x=T.GARCH(alpha=a1,beta=b1,rnd=rt,df.t=dfsim,n=samples)
    AtQ[i,]=A_tQMLE(samples,x)
    GQ[i,]=MyMLE(samples,x)$mle.N
    tQMLE[i,]=MytQMLE(samples,x,dfest=dfsim)$qmle.N
    NGQ[i,]=NGQMLE(samples,x,dfest=4,est.para=GQ[i,])
  }
  list(E.AtQ=AtQ,E.GQ=GQ,E.tQMLE=tQMLE,E.2NGQMLE=NGQ)
}
# �ص�Ĺ���
source("RMSE1000.R")
## t2.250
t2.250=finalsim(repn=200,samples=250,dfsim=2)
RMSE2.250=sqrt(RMSE1000(dat=t2.250,a=a1,b=b1))
save(RMSE2.250, file = "~\\study\\GARCH\\simulation\\RMSE2.250.RData")
rm(t2.250)
RMSE2.250

## t2.500
t2.500=finalsim(repn=200,samples=500,dfsim=2)
RMSE2.500=sqrt(RMSE1000(dat=t2.500,a=a1,b=b1))
save(RMSE2.500, file = "~\\study\\GARCH\\simulation\\RMSE2.500.RData")
rm(t2.500)
RMSE2.500


##
source("RMSE1000.R")
source("ratioVMLE.R")
test=finalsim(repn=10,samples=500,dfsim=2)
test
RMSEtest=RMSE1000(dat=test,a=a1,b=b1)
RMSEtest

## ratioVMLE(a1,b1,simdf=2,samplesize=500,repN=20)


## t2.1000
t2.1000=finalsim(repn=200,samples=1000,dfsim=2)
save(t2.1000, file = "~\\study\\GARCH\\simulation\\results\\t2.1000.RData")
RMSE2.1000=RMSE1000(dat=t2.1000,a=a1,b=b1)
save(RMSE2.1000, file =
     "~\\study\\GARCH\\simulation\\results\\RMSE2.1000.RData")
t2.1000$E.AtQ
t2.1000$E.GQ
rm(t2.1000)
RMSE2.1000

## ## t2.2000
## t2.2000=finalsim(repn=200,samples=2000,dfsim=2)
## RMSE2.2000=sqrt(RMSE1000(dat=t2.2000,a=a1,b=b1))
## save(RMSE2.2000, file = "~\\study\\GARCH\\simulation\\results\\RMSE2.2000.RData")
## rm(t2.2000)
## RMSE2.2000

## t3.500, 200rep
t3.500=finalsim(repn=200,samples=500,dfsim=3)
RMSE3.500=RMSE1000(dat=t3.500,a=a1,b=b1)
save(t3.500, file = "~\\study\\GARCH\\simulation\\results\\t3.500.RData")
save(RMSE3.500, file = "~\\study\\GARCH\\simulation\\results\\RMSE3.500.RData")
rm(t3.500)

## T3.250.200rep
t3.250=finalsim(repn=200,samples=250,dfsim=3)
RMSE3.250=RMSE1000(dat=t3.250,a=a1,b=b1)
save(t3.250, file = "~\\study\\GARCH\\simulation\\results\\t3.250.RData")
save(RMSE3.250, file = "~\\study\\GARCH\\simulation\\results\\RMSE3.250.RData")
rm(t3.250)

t3.1000=finalsim(repn=200,samples=1000,dfsim=3)
RMSE3.1000=RMSE1000(dat=t3.1000,a=a1,b=b1)
save(t3.1000, file = "~\\study\\GARCH\\simulation\\results\\t3_1000.RData")
save(RMSE3.1000, file = "~\\study\\GARCH\\simulation\\results\\RMSE3_1000.RData")
rm(t3.1000)
## finished here
RMSE3.500
RMSE3.250
RMSE3.1000
RMSE2.250
RMSE2.500
RMSE2.1000
ls()

## ######################���滹δģ��#########
## t3.2000=finalsim(repn=1000,samples=2000,dfsim=3)
## save(t3.2000, file = "~\\study\\GARCH\\simulation\\results\\t3_2000.RData")
## rm(t3.2000)

## T5.250.200rep
t5.250=finalsim(repn=200,samples=250,dfsim=5)
RMSE5.250=RMSE1000(dat=t5.250,a=a1,b=b1)
save(t5.250, file = "~\\study\\GARCH\\simulation\\results\\t5.250.RData")
save(RMSE5.250, file = "~\\study\\GARCH\\simulation\\results\\RMSE5.250.RData")
rm(t5.250)

## t5.500, 200rep
t5.500=finalsim(repn=200,samples=500,dfsim=5)
RMSE5.500=RMSE1000(dat=t5.500,a=a1,b=b1)
save(t5.500, file = "~\\study\\GARCH\\simulation\\results\\t5.500.RData")
save(RMSE5.500, file = "~\\study\\GARCH\\simulation\\results\\RMSE5.500.RData")
rm(t5.500)

## t5.1000, 200rep
t5.1000=finalsim(repn=200,samples=1000,dfsim=5)
RMSE5.1000=RMSE1000(dat=t5.1000,a=a1,b=b1)
save(t5.1000, file = "~\\study\\GARCH\\simulation\\results\\t5_1000.RData")
save(RMSE5.1000, file = "~\\study\\GARCH\\simulation\\results\\RMSE5_1000.RData")
rm(t5.1000)

## finish here 2013/7/9 1:15
RMSE5.250
RMSE5.500
RMSE5.1000
## finish here 2013/7/9 3:13

## T10.250.200rep
t10.250=finalsim(repn=200,samples=250,dfsim=10)
RMSE10.250=RMSE1000(dat=t10.250,a=a1,b=b1)
save(t10.250, file = "~\\study\\GARCH\\simulation\\results\\t10.250.RData")
save(RMSE10.250, file = "~\\study\\GARCH\\simulation\\results\\RMSE10.250.RData")
rm(t10.250)

## t10.500, 200rep
t10.500=finalsim(repn=200,samples=500,dfsim=10)
RMSE10.500=RMSE1000(dat=t10.500,a=a1,b=b1)
save(t10.500, file = "~\\study\\GARCH\\simulation\\results\\t10.500.RData")
save(RMSE10.500, file = "~\\study\\GARCH\\simulation\\results\\RMSE10.500.RData")
rm(t10.500)

## t10.1000, 200rep
t10.1000=finalsim(repn=200,samples=1000,dfsim=10)
RMSE10.1000=RMSE1000(dat=t10.1000,a=a1,b=b1)
save(t10.1000, file = "~\\study\\GARCH\\simulation\\results\\t10.1000.RData")
save(RMSE10.1000, file = "~\\study\\GARCH\\simulation\\results\\RMSE10.1000.RData")
rm(t10.1000)

## T20.250.200rep
t20.250=finalsim(repn=200,samples=250,dfsim=20)
RMSE20.250=RMSE1000(dat=t20.250,a=a1,b=b1)
save(t20.250, file = "~\\study\\GARCH\\simulation\\results\\t20.250.RData")
save(RMSE20.250, file = "~\\study\\GARCH\\simulation\\results\\RMSE20.250.RData")
rm(t20.250)

## t20.500, 200rep
t20.500=finalsim(repn=200,samples=500,dfsim=20)
RMSE20.500=RMSE1000(dat=t20.500,a=a1,b=b1)
save(t20.500, file = "~\\study\\GARCH\\simulation\\results\\t20.500.RData")
save(RMSE20.500, file = "~\\study\\GARCH\\simulation\\results\\RMSE20.500.RData")
rm(t20.500)

## t20.1000, 200rep
t20.1000=finalsim(repn=200,samples=1000,dfsim=20)
RMSE20.1000=RMSE1000(dat=t20.1000,a=a1,b=b1)
save(t20.1000, file = "~\\study\\GARCH\\simulation\\results\\t20.1000.RData")
save(RMSE20.1000, file = "~\\study\\GARCH\\simulation\\results\\RMSE20.1000.RData")
rm(t20.1000)

## finished here at 2013/7/9 10:53
ls()
RMSE10.250
RMSE10.500
RMSE10.1000
RMSE20.250
RMSE20.500
RMSE20.1000

## Ϊ��ͳһ�����¼���t2��

## t2.250, 200rep
t2.250=finalsim(repn=200,samples=250,dfsim=2)
RMSE2.250=RMSE1000(dat=t2.250,a=a1,b=b1)
save(t2.250, file = "~\\study\\GARCH\\simulation\\results\\t2.250.RData")
save(RMSE2.250, file = "~\\study\\GARCH\\simulation\\results\\RMSE2.250.RData")
rm(t2.250)

## t2.500, 200rep
t2.500=finalsim(repn=200,samples=500,dfsim=2)
RMSE2.500=RMSE1000(dat=t2.500,a=a1,b=b1)
save(t2.500, file = "~\\study\\GARCH\\simulation\\results\\t2.500.RData")
save(RMSE2.500, file = "~\\study\\GARCH\\simulation\\results\\RMSE2.500.RData")
rm(t2.500)
####################################

RMSE2.250
RMSE2.500

sink("~\\study\\GARCH\\simulation\\results\\all.RData")