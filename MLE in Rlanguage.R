x=rcauchy(1000,1)
f=function(p) sum((x-p)/(1+(x-p)^2))
out=uniroot(f,c(0,5),tol=0.000000000001)
out

##optimize() or optimise() using for 
###ֱ����һά���������ļ�С�㣬�����ڶ�����Ȼ�����ļ�ֵ��
loglike=function(p) sum(log(1+(x-p)^2))
out2=optimize(loglike,c(0,5))
out2


###����nlm()��ʹ��
obj<-function(x){
   f<-c(10*(x[2]-x[1]^2),1-x[1])
   sum(f^2)
}

x0=c(-1.2,1)
nlm(obj,x0)

##miimum�Ǻ���������Ŀ��ֵ��estimate�����ŵ�Ĺ���ֵ��gradient�������ŵ㴦������ֵ��
##Ŀ�꺯���ݶ�ֵ��code��ָ�꣬1��ʾ�����ɹ���iterations�ǵ�������