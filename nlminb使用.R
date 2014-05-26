###
x <- rnbinom(100, mu = 10, size = 10)
hdev <- function(par)
  -sum(dnbinom(x, mu = par[1], size = par[2], log = TRUE))
nlminb(c(9, 12), hdev)
nlminb(c(20, 20), hdev, lower = 0, upper = Inf)
nlminb(c(1, 2), hdev, lower = 0.001, upper = Inf)

#####正态分布的两参数MLE方法估计
x=rnorm(100,3,6)
N=function(par){
  n=length(x)
  L=-n/2*(log(2*pi*par[2]^2))-1/2/(par[2]^2)*sum((x-par[1])^2)
  return(-L)
}
hist(x)
nlminb(c(3,5),N,lower=0.000001,upper=Inf)
########线性模型的MLE方法求解
##数据的生成
xl=runif(100,5,10)
p=c(3,4.5)
yl=p[1]+p[2]*xl+rnorm(100,0,3)
hist(xl)
hist(yl)
plot(xl,yl)
dat=cbind(xl,yl)
dat
plot(dat)
###第二种数据生成，矩阵的
X<-cbind(1,runif(100))
theta.true<-c(1,7,1)
y<-X%*%theta.true[1:2] + rnorm(100)
plot(X[,2],y)
ols.lf<-function(theta,y,X){
  n<-nrow(X)
  k<-ncol(X)
  beta<-theta[1:k]
  sigma2<-theta[k+1]
  e<-y-X%*%beta
  logl<- -.5*n*log(2*pi)-.5*n*log(sigma2)-
    ((t(e)%*%e)/(2*sigma2))
  return(-logl)
} 
p<-optim(c(1,1,1),ols.lf,method="BFGS",hessian=T,y=y,X=X)
p
q<-nlminb(c(2,2,2),y=y,X=X,ols.lf)
q
#MLE方法求解
LL=function(par){
  n=length(dat[,2])
  L=-0.5*(n*log(2*pi)+n*log(par[3]^2)+1/par[3]^2*sum((dat[,2]-par[1]-par[2]*dat[,1])^2))
  return(-L)
}
hist(xl)
nlminb(c(2,5,1),LL,lower=0.000001,upper=Inf)

#####GARCH模型的条件方差分解
###第一步，生成GARCH(1,1)的序列
T.GARCH=function (alpha, beta, n = 100, rnd = rt, dft,ntrans = 100, ...) 
{
  if (!missing(beta)) 
    p = length(beta)
  else p = 0
  if (!missing(alpha)) 
    q = length(alpha) - 1
  else stop("beta is missing!")
  if (q == 0) 
    stop("Check model: q=0!")
  total.n = n + ntrans
  e = rnd(total.n,dft)
  x = double(total.n)
  sigt = x
  d = max(p, q)
  sigma2 = sum(alpha[-1])
  if (p > 0) 
    sigma2 = sigma2 + sum(beta)
  if (sigma2 > 1) 
    stop("Check model: it does not have finite variance")
  sigma2 = alpha[1]/(1 - sigma2)
  if (sigma2 <= 0) 
    stop("Check model: it does not have positive variance")
  x[1:d] = rnd(d, dft)
  sigt[1:d] = sigma2
  if (p == 0) {
    for (i in (d + 1):total.n) {
      sigt[i] = sum(alpha * c(1, x[i - (1:q)]^2))
      x[i] = e[i] * sqrt(sigt[i])
    }
  }
  else {
    for (i in (d + 1):total.n) {
      sigt[i] = sum(alpha * c(1, x[i - (1:q)]^2)) + sum(beta * sigt[i - (1:p)])
      x[i] = e[i] * sqrt(sigt[i])
    }
  }
  #return(invisible(x[(ntrans + 1):total.n]))
  list(xt=x[(ntrans + 1):total.n],sigt=sigt[(ntrans + 1):total.n])
}
###高斯分布需要用到伽马分布，来生成随机数，需要编代码，只考虑t分布先
alpha=c(0.5,0.6);beta=0.3
####随机误差项为T(3)的数据生成
garchT11.sim=T.GARCH(alpha,beta,n=100,dft=3)
length(garchT11.sim)
plot(garchT11.sim,type='l',ylab=expression(r[t]))

###第二步，对这个序列进行还原求条件方差
x=garchT11.sim
for (t in 2:n) {
  sig[t]=1+x[t-1]##
}
#{ }
x[1:d] = rnd(d, sd = sqrt(sigma2))
sigt[1:d] = sigma2
if (p == 0) {
  for (i in (d + 1):total.n) {
    sigt[i] = sum(alpha * c(1, x[i - (1:q)]^2))
    x[i] = e[i] * sqrt(sigt[i])
  }
}
else {
  for (i in (d + 1):total.n) {
    sigt[i] = sum(alpha * c(1, x[i - (1:q)]^2)) + sum(beta * 
                                                        sigt[i - (1:p)])
    x[i] = e[i] * sqrt(sigt[i])
  }
}
