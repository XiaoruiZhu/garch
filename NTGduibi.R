#    运用函数对比不同的参数估计
#    这里考虑数列是带有厚尾t分布的随机误差

NTGduibi=function(h,a,b,dfsim,dfest.t,dfest.G){
  x=T.GARCH(alpha=a,beta=b,rnd=rt,df.t=dfsim,n=h)
  sim1x=xitong(h,x)
  sim1my=MyMLE(h,x)
  sim1myt=MytQMLE(h,x,dfest=dfest.t)
  sim1myG=MyGQMLE(h,x,dfest=dfest.G)
  sim1ngq=NGQMLE(h,x,dfest=dfest.t)
  yita=sim1ngq$Yita
  result.TSA=c(sim1x$coef)
  # 对比结果合到一起
  para.true=c(a,b)
  compar.para=rbind(para.true,result.TSA,MyMLE=sim1my$mle.N,
                    MytQMLE=sim1myt$qmle.N,MyGQMLE=sim1myG$gqmle,
                    MYNGQMLE=sim1ngq$NGQMLE)
  list(com.para=compar.para,n=h,Yita=yita)
}