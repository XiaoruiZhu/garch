#    运用函数对比估计参数函数
duibi=function(h,a,b){
  #set.seed(1234567)
  x=garch.sim(alpha=a,beta=b,n=h)
  sim1x=xitong(h,x)
  sim1my=MyMLE(h,x)
  result.TSA=c(sim1x$coef)
  # 对比结果合到一起
  para.true=c(a,b)
  compar.para=rbind(para.true,result.TSA,MyMLE=sim1my$mle.N)
  list(com.para=compar.para,n=h)
}