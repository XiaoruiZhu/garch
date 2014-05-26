
###��Ȼ����

###��˹�ֲ���Ҫ�õ�٤���ֲ������������������Ҫ����룬ֻ����t�ֲ���
a=c(0.5,0.6);b=0.3
####��������ΪT(3)����������
garchT11.sim=T.GARCH(a,b,n=50,dft=3)
garchT11.sim
#hist(garchT11.sim$xt)
#plot(garchT11.sim$xt)
plot(garchT11.sim$xt,type='l',ylab=expression(r[t]))

####��ͨ��GARCH(1,1)��Ȼ����likelihood
TTL=function(para){
  n=length(x)
  sig=numeric(n);tem1=numeric(1);tem2=numeric(1)
  w=para[1]
  alpha=para[2]
  beta=para[3]
  sig[1]=w/(1-alpha-beta)
  for (t in 2:n){
    tem1=tem1+beta^(t-2)
    tem2=beta*(tem2)+x[t-1]^2  ###�����⣡��
    sig[t]=w*sig[1]+alpha*(tem2)+beta^(t-1)*sig[1]
  }
  LL=-1/2*sum(log(sig)+x^2/(sig))
  return(-LL)
}

nlminb(c(0.1,0.1,0.1),TTL,lower=-Inf,upper=Inf)

###TSA�����garch()�������
####��������ΪN����������
library(TSA)
a=c(0.5,0.6);b=0.3

##��ϵͳ��GARCH������Ľ��жԱ�
xitong=function(h,a,b){
  set.seed(h)
  x=garch.sim(alpha=a,beta=b,n=h)
  M.N=garch(x,order=c(1,1))
  summary(M.N)
}
xitong(500,a,b)

##��̬GARCH(1,1)��mle
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
      tem2=beta*(tem2)+x[t-1]^2  ###�����⣡��
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
#�Բ�ͬ�����������й��ƣ����Լ����nlminb����
MyMLE(250,a,b)
MyMLE(500,a,b)
MyMLE(2000,a,b)


##  T�ֲ���GARCH(1,1) #������ݵ����ɣ�������˹��t�����������GARCH(1,1)
source("Y:\\study!!\\ʱ������\\��Ҫѧϰ\\R CODES\\T.GARCH.R")
garchT11.sim=T.GARCH(a,b,n=1000,dft=3)
x=garchT11.sim$xt
plot(x,type='l',col=6)
mle.T=nlminb(c(0.01,0.04,0.4),TTL,lower = 0.001, upper = Inf)
mle.T