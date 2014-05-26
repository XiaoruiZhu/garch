# 先用已得的Bestdf.t来tQMLE估计参数，用参数计算e，在计算yita一堆，找到最接近1的Bestdf.t
G.EstmBestdf=function(h,t,bestdf){
  Nest=MyGQMLE(h,t,dfest=bestdf)$gqmle
  e.t=com.e(Nest,x=t)
  # 计算MLE得出的残差e.t，这个是需要来寻找最优f的参数的，yita=1作为边
  # 界！
  QG.df=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.2,1.3,1.5,1.7,1.8,2,2.2,2.4,2.6,2.8,3,3.3,3.5,3.8,4)
  samplesYITA=rep(NA,length(QG.df))
  for (i in 1:length(QG.df)){
    samplesYITA[i]=YITAGQMLE(h=h,e=e.t,dfest=QG.df[i])
  }
  NA0.5=which(samplesYITA!=0.5)
  wherebestYita=which(min(abs(samplesYITA[NA0.5]-1))==abs(samplesYITA[NA0.5]-1))
  Bdf.G=QG.df[wherebestYita]
  names(samplesYITA)=QG.df
#   yitaall=samplesYITA[(wherebestYita-3):(wherebestYita+3)]
  list(Est=Nest,B.df=Bdf.G,samYita=samplesYITA)
#        YITAall=yitaall)
}
