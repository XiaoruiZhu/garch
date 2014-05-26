# h is the length of time-series
# x is the time-series
# A_tQMLE=function(h,x){
  # 生成要用的序列，t error，parameter=,以及估计参数矩阵，best df矩阵
source("MytQMLE.R")  
source("YITAtQMLE.R")
a1=c(0.5,0.55);b1=0.4;samples=1000;dfsim=2

YITAtQMLE(samples,rt(samples,df=dfsim),dfest=dfsim)
test=rt(samples,df=dfsim)
mean(test) ; var(test) ; kurtosis(test)
YITAtQMLE(samples,test,dfest=2)  
Estm=matrix(NA,9,3)
  sampYITA=matrix(NA,9,23)
  Bdf.t=rep(NA,9)
  # 第一步！！！！！
samples

  x=T.GARCH(alpha=a1,beta=b1,rnd=rt,df.t=dfsim,n=samples)
  plot(x,type='l')
  Estm[1,]=MyMLE(samples,x)$mle.N
Estm[1,]
Est.tq=MytQMLE(samples,x,dfest=2)$qmle.N
Est.tq
  e.t1=com.e(Estm[1,],x)
  mean(e.t1);var(e.t1);kurtosis(e.t1)
  plot(e.t1,type='l')
  hist(e.t1)
normalTest(rt(samples,df=1),method='jb')  
normalTest(e.t1,method='jb')
YITAtQMLE(samples,rt(samples,df=2),dfest=2)
YITAtQMLE(samples,e.t1,dfest=10)

e.t2=com.e(Est.tq,x)
mean(e.t2);var(e.t2);kurtosis(e.t2)
plot(e.t2,type='l')
hist(e.t2)
normalTest(rt(samples,df=1),method='jb')  
normalTest(e.t2,method='jb')
YITAtQMLE(samples,rt(samples,df=2),dfest=2)
YITAtQMLE(samples,e.t2,dfest=2)

length(e.t1)
  # 计算MLE得出的残差e.t，这个是需要来寻找最优f的参数的，yita=1作为边界！
  Qt.df=c(0.3,0.5,0.7,0.9,1,1.2,1.4,1.6,1.8,2,3,4,5,6,7,8,9,10,11,15,20,30,100)
  ndf=length(Qt.df)
  samplesYITA=rep(NA,ndf)
  for (i in 1:ndf){
    samplesYITA[i]=YITAtQMLE(h=samples,e=e.t1,dfest=Qt.df[i])
  }
  # 接下来用理论的rt残差与之对比看yita大概走向
  samplesYITA
  Bdf.t[1]=Qt.df[which(min(abs(samplesYITA-1))==abs(samplesYITA-1))]
  Bdf.t[1]

  # 第二步开始估计，用的是tQMLE，其中t分布用的参数时第一步估计出来的Bestdf.t
  source('EstmBestdf.R')
  for (j in 2:9){
    EstmBdf=EstmBestdf(samples,x,bestdf=Bdf.t[j-1])
    Estm[j,]=EstmBdf$Est
    Bdf.t[j]=EstmBdf$B.df  
    sampYITA[j,]=EstmBdf$samYita
#     if (Bdf.t[j]==Bdf.t[j-1]){k=j-1;break}
  }
  Para.df=c(Estm[j-1,],Bdf.t[j-1])
  Para.df  
  Bdf.t;Estm;
  sampYITA
  #   return(Para.df)
}