x=rcauchy(1000,1)
f=function(p) sum((x-p)/(1+(x-p)^2))
out=uniroot(f,c(0,5),tol=0.000000000001)
out

##optimize() or optimise() using for 
###直接求一维变量函数的极小点，可用于对数似然函数的极值点
loglike=function(p) sum(log(1+(x-p)^2))
out2=optimize(loglike,c(0,5))
out2


###函数nlm()的使用
obj<-function(x){
   f<-c(10*(x[2]-x[1]^2),1-x[1])
   sum(f^2)
}

x0=c(-1.2,1)
nlm(obj,x0)

##miimum是函数的最优目标值，estimate是最优点的估计值，gradient是在最优点处（估计值）
##目标函数梯度值，code是指标，1表示迭代成功，iterations是迭代次数