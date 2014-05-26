geyser

attach(geyser)

hist(waiting)
#��ͼ�п��Կ�������ֲ���������̬�ֲ��Ļ�ϡ����������µķֲ�����������������
#f(x)=pN(xi;��1,��1)+(1?p)N(xi;��2,��2)
#�ú�������5������[latex p��\mu_1��\sigma_1��\mu_2��\sigma_2$��Ҫȷ����
#�����ֲ������Ķ���������Ȼ����Ϊ��
#l=��(ni=1)log{pN(xi;��1,��1)+(1?p)N(xi;��2,��2)}

#NGQMLE log-likelihood����
GQMLE<-function(params,data){
    t1<-dgamma( 
#����log-likelihood����
LL<-function(params,data){
    #����"params"��һ�����������ΰ��������������p,mu1,sigma1,
    #mu2,sigma2.
    #����"data"���ǹ۲����ݡ�
t1<-dnorm(data,params[2],params[3])
t2<-dnorm(data,params[4],params[5]) #�����dnorm()����������������̬�ܶȺ����ġ�
f<-params[1]*t1+(1-params[1])*t2 #����ܶȺ���
ll<-sum(log(f)) #log-likelihood����
return(-ll)
    #nlminb()��������С��һ��������ֵ����������Ҫ���log-
    #likeilhood������������Ҫ�ڡ�ll��ǰ�Ӹ���-���š�
}

#��hist�����ҳ���ʼֵ
hist(waiting,freq=F)
lines(density(waiting))
#��Ϻ���####optim####
geyser.res<-nlminb(c(0.5,50,10,80,10),LL,data=waiting,
lower=c(0.0001,-Inf,0.0001,-Inf,-Inf,0.0001),
upper=c(0.9999,Inf,Inf,Inf,Inf))
#��ʼֵΪp=0.5,mu1=50,sigma1=10,mu2=80,sigma2=10
#LL�Ǳ���С���ĺ�����
#data������õ�����
#lower��upper�ֱ�ָ���������Ͻ���½硣

#�鿴��ϵĲ���

geyser.res$par
geyser.res


#��ϵ�Ч��
X<-seq(40,120,length=100)#�������ƵĲ���
p<-geyser.res$par[1]
mu1<-geyser.res$par[2]
sig1<-geyser.res$par[3]
mu2<-geyser.res$par[4]
sig2<-geyser.res$par[5]
#�����ƵĲ�����������ԭ�ܶȺ�����
f<-p*dnorm(X,mu1,sig1)+(1-p)*dnorm(X,mu2,sig2)
#�������ݵ�ֱ��ͼ
hist(waiting,probability=T,col=0,ylab="Density",
ylim=c(0,0.04),xlab="Eruption waiting times")
    #������ϵ�����
lines(X,f)