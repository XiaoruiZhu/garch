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




#####�����趨##########
a1=c(0.02,0.6);b1=0.3
a2=c(0.02,0.05);b2=0.9
h=1000;dfsim=3
x=T.GARCH(alpha=a1,beta=b1,rnd=rt,df.t=dfsim,n=h)
source('MyMLE.R')
MyMLE(h,x)
source('MyMLE.R')
m2=MyMLE(h,x)
m2
plot(x,type='l')
NGQMLE(h,x,dfest=4)

##### ��βGARCHģ�͵�nlminb��TSA���ƽ��������������Ч��######
# draw the picture of comparation between TSA.garch and MyMLE with the number 100 to 300
source("MSEbysam.R")
RMSE1=MSEbysam(maxs=1000,gap=250,repN=1000,a1,b1)
RMSE2=MSEbysam(maxs=1000,gap=250,repN=1000,a2,b2)
RMSE1;RMSE2

source("plotRMSEbysam.R")
plot.RMSEbysam(da.tsa=RMSE1$RMSE.TSA,da.mle=RMSE1$RMSE.MLE)
RMSE1$RMSE.TSA
RMSE1$RMSE.MLE



# ��βGARCH�ĶԱ�(inovation is still normal distribution)
library(lavaan)
duibi(1000,a1,b1)
duibi(500,a2,b2)$com.para[3,]

######���Խ�����Ʋ�������ֵͼ�������������仯
# MSEbysam=function(maxs,gap,repN,a,b)�������



#####��������Yitaֵ#######
library(pgnorm)
source('YITAGQMLE.R')
source('theoryYita.R')
simc.t=c(2,4,7,10)
simc.g=c(0.3,0.5,1,2)
gc=c(0.1,0.2,0.3,0.4,0.5,0.6,1,1.2,1.4,1.6,1.8,2,seq(from=2,to=4,by=0.5))
tc=c(seq(from=0.5,to=1.9,by=0.1),seq(from=2,to=8.5,by=0.5),seq(from=9,to=20,by=1))
length(tc)
# �������۵�$\yita$ֵ���Ա�t error��t quasi likelihood function�õ���
# \yita
# ����GED ERROR��GED likelihood function�õ���yita
the.yita1=theoryYita(n=50000,dfsim.error=simc.t,rn=rt,dfest.G=gc,dfest.t=tc)
the.yita1
theo.G.T1=round(the.yita1$G.T,digits=3)
theo.T.T1=round(the.yita1$T.T,digits=3)
theo.G.T1
theo.T.T1
# �����������أ���Ҫ������������������кܶຬ1��
the.yita2=theoryYita(n=50000,dfsim.error=simc.g,rn=rpgnorm,dfest.G=gc,dfest.t=tc)
theo.G.G2=round(the.yita2$G.T,digits=3)
theo.T.G2=round(the.yita2$T.T,digits=3)
theo.G.G2
theo.T.G2

# ��ʼ��ͼ

plot(tc[1:33],theo.T.T1[1:33,1],type='l',
     lty=1,lwd=2,xlim=c(0.5,10),ylim=c(0.5,1.6),
     xlab="Degree of Freedom",ylab=expression(eta))
axis(1,0:10)
axis(2,seq(from=0,to=2,by=0.1))
# lines(tc,theo.T.T1[,1],lty=2,lwd=2)
lines(tc,theo.T.T1[,2],lty=2,lwd=2)
lines(tc,theo.T.T1[,3],lty=5,lwd=2)
lines(tc,theo.T.T1[,4],lty=4,lwd=2)
#lines(tc,theo.T.G2[,1],lty=1,pch=20,col=1,lwd=1)
lines(tc,theo.T.G2[,2],lty=1,pch=20,col=2,lwd=1)
lines(tc,theo.T.G2[,3],lty=2,pch=20,col=3,lwd=1)
lines(tc,theo.T.G2[,4],lty=5,pch=20,col=4,lwd=1)
abline(h=1,lwd=2)
legend(1, 1.6, c("t2", "t4", "t6","t10","gg0.5","gg1","gg2"),
       lty = c(1,2,5,4,1,2,5),lwd=c(2,2,2,2,1,1,1),
       merge = FALSE, bg='white',text.font=2,adj = c(0,0.3))
title(main="students's t distribution innovation with the changing t quasi likelihood",
      font.main=3)
# gg quasi likelihood function ��eta
theo.G.G2
theo.G.T1
plot(gc[1:16],theo.G.G2[1:16,4],type='l',
     ylim=c(0.5,1.6),xlim=c(0.1,2.5),lty=1,lwd=2,
     xlab="degree freedom",ylab=expression(eta)) #gg2
axis(1,seq(from=0,to=2.5,by=0.1))
axis(2,seq(from=0.5,to=1.6,by=0.1))
lines(gc[1:14],theo.G.G2[1:14,3],lty=2,lwd=2) #gg1
lines(gc[1:9],theo.G.G2[1:9,2],lty=5,lwd=2) #gg0.5
#lines(gc[1:7],theo.G.G2[1:7,1],lty=4,lwd=2) #gg0.3
# t�ֲ���Ϣ��������gg likelihood function���Ƶ�eta
lines(gc[1:7],theo.G.T1[1:7,1],lty=1,pch=20,col=2,lwd=1) #t2
lines(gc[1:9],theo.G.T1[1:9,2],lty=2,pch=20,col=3,lwd=1) #t4
lines(gc[1:12],theo.G.T1[1:12,3],lty=5,pch=20,col=4,lwd=1) #t7
abline(h=1,lwd=2)
legend(0.3, 1.6, c("gg2", "gg1", "gg0.5","t2","t4","t7"),
       lty = c(1, 2, 5,1,2,5),lwd=c(2,2,2,1,1,1),
       merge = TRUE, bg='white',text.font=2,
       adj = c(0,0.3))
title(main="GED innovation with the changing GED quasi likelihood",
      font.main=3)

# lines(tc,theo.T.T[,1],lty=7)
# ��������theory yita����������Ȼ��ȴ���ͼ

# �������ǹ��Ƶ�һ��֮���yita


# ����yita���жԱȣ���Բ�ͬ��error distribution�ԱȲ�ͬ��quasi likelihood
source("comyita.R")
source("simYITA.R")
a1=c(0.5,0.6);b1=0.3;h=1000
simYITA(n=h,a=a1,b=b1,dfsim=30)
simYITA(n=h,a=a1,b=b1,dfsim=5)

######################################
###������ã�������0.5�ģ����ֵ֮���С�ˡ�������
##############  t(5) innovation distribution  #######################


BSComYita(x,dfest.G=gc,dfest.t=tc,bootN=500)
MyMLE(h=h,x=x)
NGQMLE(h=h,x=x,dfest)


# ��βGARCH�ĶԱ�(inovation is still normal distribution)
duibi(500,a2,b2)
####��������ΪT(3)����������
### heavy-tailed GARCH series with parameter (0.5,0.6,0.3), and heavy-tailed innovation
garcht.sim=T.GARCH(a1,b1,n=50,dft=3)
kurtosis(garcht.sim)  ## heavy-tailed series
### weak-tailed GARCH series with parameter (0.02,0.05,0.9), and heavy-tailed innovation
garcht.sim=T.GARCH(a2,b2,n=50,dft=3)
kurtosis(garcht.sim)
plot(garcht.sim$xt,type='l',ylab=expression(r[t]))


####NGQMLE������######



### ������t(20)�����ֹ��Ʒ����Ա�
source("NTGduibi.R")
# ���ɵ�innovation��t distribution
NTGduibi(1000,a1,b1,dfsim=3,dfest.t=7,dfest.G=1)
NTGduibi(1000,a2,b2,dfsim=3,dfest.t=7,dfest.G=1)

NTGduibi(1000,a1,b1,dfsim=20,dfest.t=20,dfest.G=1.4)
NTGduibi(2000,a1,b1,dfsim=20,dfest.t=20,dfest.G=1.4)

###���㹤�߱�����Fan���¶Աȣ�����dfsim������ʹ�õ���Ϣt�ֲ�������dfest.G�Ǹ�˹QMLE��
###������dfest.t��t�ֲ�����ȻQMLE�����ɶ�

#############################

# ��Ϣ�ֲ���t�ֲ������
h=500
gc=c(0.2,0.6,1,1.4,1.8)
tc=c(2.5,3,4,5,7,11,20,30)

# ��֤�˺�Fan�������м�����һ���ģ�û�����⣬���ǻ������²��������Կ����ö�ι�yita�������Ƿ�������
# ���Ƕ�μ��㿴������ֵ
source('BSComYita.R')
BSComYita(h=h,a=a1,b=b1,dfsim=3,dfest.G=gc,dfest.t=tc,bootN=500)
# ��Ϊ����ĶԱ��õĲ���ͬһ�����ݣ��б�Ҫ����������������������ͬһ������������н��жԱ�
source('duibiYita.R')
duibiYita(h=h,a=a1,b=b1,dfsim=3,dfest.G=gc,dfest.t=tc,bootN=500)


#####�����ò���####'
source("T.GARCH.R")
source("MyMLE.R")
source("MyGQMLE.R")
h=1000
t=T.GARCH(alpha=a1,beta=b1,rnd=rt,df.t=3,n=h)
plot(t,type='l')
MyMLE(h,x=t)$mle.N
xitong(h,x=t)
MyGQMLE(h,x=t,dfest=1)$gqmle
MytQMLE(h,x=t,dfest=3)$qmle.N
A_tQMLE(h,x=t)

e.t=com.e(estpara,x=t)
hist(e.t)
kurtosis(e.t)
YITAGQMLE(h=h,e=e.t,dfest=1.8)
YITAtQMLE(h=h,e=e.t,dfest=3)

hist(rt(1000,3))