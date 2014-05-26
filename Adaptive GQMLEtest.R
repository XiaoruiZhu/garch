###------------------ǰ�ڰ��������-------------------------------------
#ϵͳ��TSA������������ͨ��������ΪN��GARCH����
  library(TSA)
  library(tseries)
  library(pgnorm)

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

h=1500

a1=c(0.02,0.6);b1=0.3;dfsim=2
a2=c(0.02,0.05);b2=0.9
# ����ģ�⣬ȷʵֻҪ��eta=1��Ӧ�Ľ��й��ƣ����ƽ�������Ƶġ�
data1=G.GARCH(alpha=a1,beta=b1,rnd=rpgnorm,df.G=1,n=h)
data2=T.GARCH(alpha=a1,beta=b1,rnd=rt,df.t=5,n=h)
plot(data1,type='l')
plot(data2,type='l')
# GARCH(1,1) - use default omega and specify alpha/beta
# library(fGarch)
# spec = garchSpec(model = list(omega = 0.02,alpha = 0.6, beta = 0.3))
# testx=garchSim(spec, n = 500)
# plot(testx,type='l')

## library(fBasics)
mean(x);var(x);kurtosis(x)
## A_tQMLE(h,x)
xitong(h,x=data)
MyMLE(h,x=data)
MytQMLE(h,x=data2,dfest=5)
MyGQMLE(h,x=data2,dfest=1.2)
library(fGarch)
garchFit(formula=~garch(1,1),data=data)
A_tQMLE(h=h,x=data)

###yita
t=T.GARCH(alpha=a1,beta=b1,rnd=rt,df.t=10,n=h)
plot(t,type='l')
A_tQMLE(h=h,x=t)
####
sim1my=MyMLE(h,x=t)
sim1my
estpara=sim1my$mle.N
e.t=com.e(estpara,x=t)
plot(e.t,type='l')
source("YITAGQMLE.R")
e.t1=rt(n=2000,df=3)
plot(e.t1,type='l')
YITAtQMLE(h=h,e=e.t,dfest=78)
estpara2=MytQMLE(h=h,x=t,dfest=78)$qmle.N
estpara2
e.t2=com.e(estpara2,x=t)
plot(e.t2,type='l')
YITAtQMLE(h=h,e=e.t2,dfest=8)
estpara3=MytQMLE(h=h,x=t,dfest=8)$qmle.N
estpara3
e.t3=com.e(estpara3,x=t)
plot(e.t3,type='l')
YITAtQMLE(h=h,e=e.t3,dfest=4.5)
####


simYITA(n=h,a=a1,b=b1,dfsim=4)

## round(MyMLE(h,x)$mle.N,digits=3)
## MytQMLE(h,x,dfest=dfsim)

## ������ͨ��garch��MYMLE�ܷ����׼
## library(TSA)
## xtest=garch.sim(alpha=a1,beta=b1,n=h)
## plot(xtest,type='l')
## mean(xtest);var(xtest);kurtosis(xtest)
## round(MyMLE(h,xtest)$mle.N,digits=3)
## MytQMLE(h,xtest,dfest=100)

# ����Ҫ�õ����У�t error��parameter=,�Լ����Ʋ�������best df����

Estm=matrix(NA,11,3)
Bdf.G=rep(NA,11)
Bdf.G
# ��һ������������
sim1my=MyMLE(h,x)
Estm[1,]=sim1my$mle.N
Estm[1,]
source("YITAtQMLE.R")
source("YITAGQMLE.R")
source("com.e.R")
e.t=com.e(Estm[1,],x)
max(e.t);min(e.t)
mean(e.t); var(e.t); kurtosis(e.t);## plot(e.t,type='l')
hist(e.t)
library(fBasics)
normalTest(e.t,method='jb')
# ����MLE�ó��Ĳв�e.t���������Ҫ��Ѱ������f�Ĳ����ģ�yita=1��Ϊ�߽磡
QG.df=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.2,1.3,1.5,1.7,1.8,2,2.2,2.4,2.6,2.8,3,3.3,3.5,3.8,4)
samplesYITA=rep(NA,length(QG.df))
Bdf.G[1]=5
for (i in 1:length(QG.df)){
  samplesYITA[i]=YITAGQMLE(h=h,e=e.t,dfest=QG.df[i])
}
# �����������۵�rt�в���֮�Աȿ�yita�������
samplesYITA
na0.5=which(samplesYITA!=0.5)
na0.5
wherebestYita=which(min(abs(samplesYITA[na0.5]-1))==abs(samplesYITA[na0.5]-1))
Bdf.G[2]=QG.df[wherebestYita]
Bdf.G[2]
names(samplesYITA)=QG.df
samplesYITA

# �ڶ�����ʼ���ƣ��õ���tQMLE������t�ֲ��õĲ���ʱ��һ�����Ƴ�����Bestdf.t
source("G.EstmBestdf.R");source("MyGQMLE.R")
G.EstmBestdf(h,x,bestdf=Bdf.G[2])
MyGQMLE(h,x,dfest=0.5)
MyMLE(h,x)

Bdf.G[2]
for (j in 2:11){
  EstmBdf=G.EstmBestdf(h,x,bestdf=Bdf.G[j])
  Estm[j,]=EstmBdf$Est
  Bdf.G[j+1]=EstmBdf$B.df
      if ((Bdf.G[j]==Bdf.G[j-1])|(j==11)){
      k=j
      break}
}
round(Estm,digits=4)
Bdf.G

## estpara7=Estm[7,]
## estpara7
## e.t=com.e(estpara7,x)
## max(e.t);min(e.t)
## mean(e.t); var(e.t); kurtosis(e.t);plot(e.t,type='l')
## library(fBasics)
## normalTest(e.t,method='jb')

#���Ա�

dfsim
MyMLE(h,x)
NGQMLE(h,x,dfest=dfsim,Estm[1,])
source('MytQMLE.R')
testEst=MytQMLE(h,x,dfest=dfsim)$qmle.N
testEst

###���Գ�������ĵط���ֱ��һ��df����̫�쳬������ʵ������t5ֱ�ӵ�t3�������������
## testEst=MytQMLE(h,x,dfest=15)$qmle.N
## testEst
## teste.t=com.e(testEst,x)
## mean(e.t); var(e.t); kurtosis(e.t)
## mean(teste.t); var(teste.t); kurtosis(teste.t)
## library(fBasics)
## normalTest(e.t,method='jb')
## normalTest(teste.t,method='jb')
## # ����MLE�ó��Ĳв�e.t���������Ҫ��Ѱ������f�Ĳ����ģ�yita=1��Ϊ�߽磡
## Qt.df=c(0.3,0.5,0.7,0.9,1,1.2,1.4,1.6,1.8,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,8,9,10,11,15,20,30,100)
## samplesYITA=rep(NA,length(Qt.df))
## Bdf.t[1]=200
## for (i in 1:length(Qt.df)){
##   samplesYITA[i]=YITAtQMLE(h=h,e=teste.t,dfest=Qt.df[i])
## }
## # �����������۵�rt�в���֮�Աȿ�yita�������
## wherebestYita=which(min(abs(samplesYITA-1))==abs(samplesYITA-1))
## Bdf.t[2]=Qt.df[wherebestYita]
## Bdf.t[2]
## samplesYITA



ls()
rm(Bdf.t,Bdf.t3,Bdf.t4,Bdf.t5,Bdf.t6,Bdf.t7,t,est2,Estm3,Estm4,Estm5,Estm6,Estm7)

# ����������������������
#######test######
e.tfinal=com.e(Estm5,x=t)
plot(e.tfinal,type='l')
kurtosis(e.tfinal)
kurtosis(rt(h,dfsim))
# �ܺã���Ӧ�˲в�ӽ�t(2)���ˣ�����������ӳ��ֻ��һ�������Բ�������ȫ�����ʵ�ֲ�




# �����ʱ���ã������ñ��취ֱ����23�Σ��ó������һ��yita�ҵ���Ӧ��t�ֲ����ĸ������ģ�
# Ȼ�������������tQMLE������һ�����ƣ�����yita�ٹ�
yi.t.e=matrix(NA,28,2)
yi.t.e[1,1]=30
for (j in 1:27){
  yi.t.e[j,2]=YITAtQMLE(h=h,e=e.t,dfest=yi.t.e[j,1])
  if (yi.t.e[j,2]<1) {
    yi.t.e[j+1,1]=yi.t.e[j,1]-1
  }
  else yi.t.e[j+1,1]=yi.t.e[j,1]+1
}
yi.t.e