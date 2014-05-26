
###似然函数

###高斯分布需要用到伽马分布，来生成随机数，需要编代码，只考虑t分布先
a=c(0.5,0.6);b=0.3
####随机误差项为T(3)的数据生成
garchT11.sim=T.GARCH(a,b,n=50,dft=3)
garchT11.sim
#hist(garchT11.sim$xt)
#plot(garchT11.sim$xt)
plot(garchT11.sim$xt,type='l',ylab=expression(r[t]))

####普通的GARCH(1,1)似然函数likelihood
TTL=function(para){
  n=length(x)
  sig=numeric(n);tem1=numeric(1);tem2=numeric(1)
  w=para[1]
  alpha=para[2]
  beta=para[3]
  sig[1]=w/(1-alpha-beta)
  for (t in 2:n){
    tem1=tem1+beta^(t-2)
    tem2=beta*(tem2)+x[t-1]^2  ###有问题！！
    sig[t]=w*sig[1]+alpha*(tem2)+beta^(t-1)*sig[1]
  }
  LL=-1/2*sum(log(sig)+x^2/(sig))
  return(-LL)
}

nlminb(c(0.1,0.1,0.1),TTL,lower=-Inf,upper=Inf)

###TSA包里的garch()函数结果
####随机误差项为N的数据生成
library(TSA)
a=c(0.5,0.6);b=0.3

##与系统的GARCH求出来的进行对比
xitong=function(h,a,b){
  set.seed(h)
  x=garch.sim(alpha=a,beta=b,n=h)
  M.N=garch(x,order=c(1,1))
  summary(M.N)
}
xitong(500,a,b)

##正态GARCH(1,1)的mle
MyMLE=function(h,a,b){
  set.seed(h)
  x=garch.sim(alpha=a,beta=b,n=h)
  TTL=function(para){
    n=length(x)
    sig=numeric(n);tem1=numeric(1);tem2=numeric(1)
    w=para[1]
    alpha=para[2]
    beta=para[3]
    sig[1]=w/(1-alpha-beta)
    for (t in 2:n){
      tem1=tem1+beta^(t-2)
      tem2=beta*(tem2)+x[t-1]^2  ###有问题！！
      sig[t]=w*sig[1]+alpha*(tem2)+beta^(t-1)*sig[1]
    }
    LL=-1/2*sum(log(sig)+x^2/(sig))
    return(-LL)
  }
  #plot(c(1:500),x,type='l')
  mle.N=nlminb(c(0.1,0.1,0.1),TTL,lower=-Inf,upper=Inf)
  #mle.N
  list(mle.N=mle.N)
}
#对不同的样本数进行估计，用自己编的nlminb方法
MyMLE(250,a,b)
MyMLE(500,a,b)
MyMLE(2000,a,b)


##  T分布的GARCH(1,1) #随机数据的生成，包括高斯，t等随机误差项的GARCH(1,1)
source("Y:\\study!!\\时间序列\\重要学习\\R CODES\\T.GARCH.R")
garchT11.sim=T.GARCH(a,b,n=1000,dft=3)
x=garchT11.sim$xt
plot(x,type='l',col=6)
mle.T=nlminb(c(0.01,0.04,0.4),TTL,lower = 0.001, upper = Inf)
mle.T
