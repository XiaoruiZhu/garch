geyser

attach(geyser)

hist(waiting)
#从图中可以看出，其分布是两个正态分布的混合。可以用如下的分布函数来描述该数据
#f(x)=pN(xi;μ1,σ1)+(1?p)N(xi;μ2,σ2)
#该函数中有5个参数[latex p、\mu_1、\sigma_1、\mu_2、\sigma_2$需要确定。
#上述分布函数的对数极大似然函数为：
#l=∑(ni=1)log{pN(xi;μ1,σ1)+(1?p)N(xi;μ2,σ2)}

#NGQMLE log-likelihood函数
GQMLE<-function(params,data){
    t1<-dgamma( 
#定义log-likelihood函数
LL<-function(params,data){
    #参数"params"是一个向量，依次包含了五个参数：p,mu1,sigma1,
    #mu2,sigma2.
    #参数"data"，是观测数据。
t1<-dnorm(data,params[2],params[3])
t2<-dnorm(data,params[4],params[5]) #这里的dnorm()函数是用来生成正态密度函数的。
f<-params[1]*t1+(1-params[1])*t2 #混合密度函数
ll<-sum(log(f)) #log-likelihood函数
return(-ll)
    #nlminb()函数是最小化一个函数的值，但我们是要最大化log-
    #likeilhood函数，所以需要在“ll”前加个“-”号。
}

#用hist函数找出初始值
hist(waiting,freq=F)
lines(density(waiting))
#拟合函数####optim####
geyser.res<-nlminb(c(0.5,50,10,80,10),LL,data=waiting,
lower=c(0.0001,-Inf,0.0001,-Inf,-Inf,0.0001),
upper=c(0.9999,Inf,Inf,Inf,Inf))
#初始值为p=0.5,mu1=50,sigma1=10,mu2=80,sigma2=10
#LL是被最小化的函数。
#data是拟合用的数据
#lower和upper分别指定参数的上界和下界。

#查看拟合的参数

geyser.res$par
geyser.res


#拟合的效果
X<-seq(40,120,length=100)#读出估计的参数
p<-geyser.res$par[1]
mu1<-geyser.res$par[2]
sig1<-geyser.res$par[3]
mu2<-geyser.res$par[4]
sig2<-geyser.res$par[5]
#将估计的参数函数代入原密度函数。
f<-p*dnorm(X,mu1,sig1)+(1-p)*dnorm(X,mu2,sig2)
#作出数据的直方图
hist(waiting,probability=T,col=0,ylab="Density",
ylim=c(0,0.04),xlab="Eruption waiting times")
    #画出拟合的曲线
lines(X,f)
